module Compiler.OCaml.Codegen

import Compiler.ANF
import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.LambdaLift

import Core.Context
import Core.Directory
import Core.Name
import Core.Options
import Core.TT

import Utils.Hex
import Utils.Path

-- from libs
import Data.List
import Data.Maybe
import Data.NameMap
import Data.Strings
import Data.Vect

import System
import System.Directory
import System.File
import System.Info

%default covering

-- Reference label for keeping track of loaded external libraries
data Loaded : Type where

-- Label for noting which struct types are declared
data Structs : Type where

data Emitted : Type where


ocamlString : String -> String
ocamlString s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c || c =='_'
                  then cast c
                  else "C" ++ show (cast {to=Int} c)

export
ocamlName : Name -> String
ocamlName (NS ns n) = "ns_" ++ showSep "_" ns ++ "_" ++ ocamlName n
ocamlName (UN n) = ocamlString n
ocamlName (MN n i) = ocamlString n ++ "_" ++ show i
ocamlName (PV n d) = "pat__" ++ ocamlName n
ocamlName (DN _ n) = ocamlName n
ocamlName (RF n) = "rf__" ++ ocamlString n
ocamlName (Nested (i, x) n) = "n__" ++ show i ++ "_" ++ show x ++ "_" ++ ocamlName n
ocamlName (CaseBlock x y) = "case__" ++ show x ++ "_" ++ show y
ocamlName (WithBlock x y) = "with__" ++ show x ++ "_" ++ show y
ocamlName (Resolved i) = "fn__" ++ show i

quotedName : Name -> String
quotedName name = fastAppend ["\"", ocamlName name, "\""]

emit : { auto e : Ref Emitted (List String)} -> String -> Core ()
emit str = do
    xs <- get Emitted
    put Emitted (str :: xs)

varName : AVar -> String
varName (ALocal i) = "v" ++ show i
varName ANull = "V"

varList : String -> List AVar -> String
varList sep vars = showSep sep (map varName vars)

argList : String -> List Int -> String
argList sep vars = showSep sep (map (\i => "v"++show i) vars)

comment : String -> String
comment str = fastAppend ["(* ", str, " *)\n"]

nameList : List Name -> String
nameList names = showSep " " (map ocamlName names)

compileConstant : {auto e : Ref Emitted (List String)} ->
                  Constant -> Core ()
compileConstant (I n) = emit $ fastAppend ["(I ", show n, ")" ]
compileConstant (BI n) = emit $ fastAppend ["(I ", show n, ")" ]
-- compileConstant (BI n) = emit $ fastAppend ["(BI (Z.of_string \"", show n, "\"))"]
compileConstant (Str str) = emit $ fastAppend ["(S ", show str, ")"]
compileConstant (Ch c) = emit $ fastAppend ["(C ", show c, ")"]
compileConstant (Db d) = emit $ fastAppend ["(D ", show d, ")"]
compileConstant WorldVal = emit $ "(TT false)"
compileConstant _ = emit $ "(TT true)"

getOp : PrimFn arity -> Maybe String
getOp (Add ty) = Just "idris_add"
getOp (Sub ty) = Just "idris_sub"
getOp (Mul ty) = Just "idris_mul"
getOp (Div ty) = Just "idris_sub"
getOp (Mod ty) = Just "idris_mod"
getOp (Neg ty) = Just "idris_neg"
getOp (ShiftL ty) = Just "idris_shl"
getOp (ShiftR ty) = Just "idris_shr"
getOp (BAnd ty) = Just "idris_and"
getOp (BOr ty) = Just "idris_or"
getOp (BXOr ty) = Just "idris_xor"
getOp (LT ty) = Just "idris_lt"
getOp (LTE ty) = Just "idris_lte"
getOp (EQ ty) = Just "idris_eq"
getOp (GTE ty) = Just "idris_gte"
getOp (GT ty) = Just "idris_gt"
getOp StrLength = Just "idris_strlen"
getOp StrHead = Just "idris_strhead"
getOp StrTail = Just "idris_strtail"
getOp StrIndex = Just "idris_strindex"
getOp StrCons = Just "idris_strappend"
getOp StrAppend = Just "idris_strrev"
getOp StrSubstr = Just "idris_substr"
getOp DoubleExp = Just "idris_exp"
getOp DoubleLog = Just "idris_log"
getOp DoubleSin = Just "idris_sin"
getOp DoubleCos = Just "idris_cos"
getOp DoubleTan = Just "idris_tan"
getOp DoubleASin = Just "idris_asin"
getOp DoubleACos = Just "idris_acos"
getOp DoubleATan = Just "idris_atan"
getOp DoubleSqrt = Just "idris_sqrt"
getOp DoubleFloor = Just "idris_floor"
getOp DoubleCeiling = Just "idris_ceil"
getOp Crash = Just "idris_crash"

getOp (Cast ty StringType) = Just "idris_to_string"
getOp (Cast ty IntType) = Just "idris_to_int"
getOp (Cast ty IntegerType) = Just "idris_to_int"
getOp (Cast ty DoubleType) = Just "idris_to_double"
getOp (Cast IntType CharType) = Just "idris_to_char"
getOp _ = Nothing

iota : Nat -> Int -> List Int
iota Z _ = []
iota (S n) i = i::iota n (i+1)

mutual

    compileOp : {auto ctxt : Ref Ctxt Defs} ->
                {auto e : Ref Emitted (List String)} ->
                PrimFn arity -> Vect arity AVar -> Core ()
    compileOp BelieveMe [_,_,exp] = emit $ varName exp
    compileOp op args = do
        case getOp op of
            Just fn => do emit $ fastAppend["(", fn, " "]
                          emit $ varList " " (toList args)
                          emit ")"
            Nothing =>  coreLift $ putStrLn ("Can't handle " ++ show op)

    compilePrim : {auto ctxt : Ref Ctxt Defs} ->
                  {auto e : Ref Emitted (List String)} ->
                  Name -> List AVar -> Core ()
    compilePrim name args = coreLift $ putStrLn ("Unknown primitive " ++ show name)

    compileConAlt : {auto ctxt : Ref Ctxt Defs} ->
                     {auto e : Ref Emitted (List String)} ->
                     AConAlt -> Core ()
    compileConAlt (MkAConAlt _ (Just i) args exp) = do
        emit $ fastAppend [" | CON { tag=", show i, "; vals=[| "]
        emit (argList "; " args)
        emit "|] -> "
        compileExp exp
        emit "\n"
    compileConAlt (MkAConAlt name Nothing args exp) = do
        emit $ fastAppend [" | NCON { name=", show name, "; args=[| "]
        emit (argList "; " args)
        emit "|] -> "
        compileExp exp
        emit "\n"

    compileConCase : {auto ctxt : Ref Ctxt Defs} ->
                     {auto e : Ref Emitted (List String)} ->
                     AVar -> List AConAlt -> Maybe ANF  -> Core ()
    compileConCase sc alts def = do
        emit $ fastAppend ["match ", varName sc, " with\n"]
        traverse compileConAlt alts
        compileDefault def

    compileDefault :{auto ctxt : Ref Ctxt Defs} ->
                     {auto e : Ref Emitted (List String)} ->
                     Maybe ANF -> Core ()
    compileDefault Nothing = pure ()
    compileDefault (Just exp) = do
        emit " | _ -> "
        compileExp exp
        emit "\n"

    compileConstAlt : {auto ctxt : Ref Ctxt Defs} ->
                     {auto e : Ref Emitted (List String)} ->
                     AConstAlt -> Core ()
    compileConstAlt (MkAConstAlt c expr) = do
        emit " | "
        compileConstant c
        emit " -> "
        compileExp expr
        emit "\n"

    compileConstCase : {auto ctxt : Ref Ctxt Defs} ->
                     {auto e : Ref Emitted (List String)} ->
                     AVar -> List AConstAlt -> Maybe ANF -> Core ()
    compileConstCase sc alts def = do
        emit $ fastAppend ["match ", varName sc, " with\n"]
        traverse compileConstAlt alts
        compileDefault def

    compileExp : {auto ctxt : Ref Ctxt Defs} ->
                {auto e : Ref Emitted (List String)} ->
                ANF -> Core ()
    compileExp (AV _ var) = emit $ varName var
    compileExp (AAppName _ name args) = do
        emit $ fastAppend ["(", ocamlName name, " "]
        emit $ varList " " args
        emit ")"
    compileExp (AUnderApp _ n m args) = do
        let argList = map (\i => "c"++show i) (iota m 0)
        traverse (\v => emit $ fastAppend ["(FUN (fun ", v, " -> "]) argList
        emit $ fastAppend [ocamlName n, " "]
        emit $ varList " " args
        emit " "
        emit $ showSep " " argList
        emit $ (pack (replicate (2*length argList) ')'))
        emit "\n"
    compileExp (AApp _ f arg) =
        emit $ fastAppend ["(idris_apply ", varName f, " ", varName arg, ")"]
    compileExp (ALet _ var exp body) = do
        emit $ fastAppend ["let v", show var, " = "]
        compileExp exp
        emit " in "
        compileExp body
        emit "\n"
    compileExp (ACon fc _ (Just n) args) = do
        emit $ fastAppend ["(CON { tag=", show n, "; vals=[| "]
        emit $ varList "; " args
        emit "|] })"
    compileExp (ACon fc name Nothing args) = do
        emit $ fastAppend ["(NCON { name=", quotedName name, "; args=[| "]
        emit $ varList "; " args
        emit "|] })"
    compileExp (AOp fc fn args) = compileOp fn args
    compileExp (AExtPrim fc name args) = compilePrim name args

    compileExp (AConCase fc sc alts def) = compileConCase sc alts def
    compileExp (AConstCase fc sc alts def) = compileConstCase sc alts def
    compileExp (APrimVal fc c) = compileConstant c
    compileExp (AErased fc) = emit "()"
    compileExp (ACrash fc msg) =
        emit $ fastAppend ["(raise (Idris_exception ", show msg, "))"]
    compileExp e = do
        coreLift $ putStrLn $ fastAppend ["I don't know how to compile ", show e]

compileFun : {auto ctxt : Ref Ctxt Defs} ->
             {auto e : Ref Emitted (List String)} ->
             Name -> List Int -> ANF -> Core ()
compileFun name args exp = do
    emit $ fastAppend [ "let rec ", ocamlName name, " ", argList " " args, " =\n" ]
    compileExp exp
    emit ";;\n\n"

compileDef' : {auto ctxt : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
             {auto e : Ref Emitted (List String)} ->
            Name -> ANFDef -> Core ()
compileDef' name (MkAFun args exp) = compileFun name args exp
compileDef' name ndef = pure ()

compileDef : {auto ctxt : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
              (Name, ANFDef) -> Core String
compileDef (name, def) = do
    e <- newRef {t = List String} Emitted []
    compileDef' name def
    code <- get Emitted
    pure $ fastAppend (reverse code)

header : () -> String
header _ = "(* placeholder for header *)\n"

footer : () -> String
footer _ = "(* placeholder for footer *)\n"

compile : Ref Ctxt Defs -> (execDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile ctxt dir term out = do
    coreLift $ putStrLn "Compiling OCaml!"
    coreLift $ putStrLn dir
    coreLift $ putStrLn out
    let outfile = out <.> "ml" -- todo: add dir
    directives <- getDirectives OCaml
    cdata <- getCompileData Cases term
    let ndefs = namedDefs cdata
    let adefs = anf cdata

    l <- newRef {t = List String} Loaded ["libc", "libc 6"]
    s <- newRef {t = List String} Structs []

    pieces <- traverse compileDef adefs
    support <- readDataFile "ocaml/support.ml"
    let code = fastAppend pieces
    let src = fastAppend [header (), support, code, footer ()]
    Right () <- coreLift $ writeFile outfile src
            | Left err => throw (FileErr outfile err)
    coreLift $ chmodRaw outfile 0o755
    pure $ Just "(* TBD *)"

execute : Ref Ctxt Defs -> (execDir : String) -> ClosedTerm -> Core ()
execute ctxt dir term = pure ()

export
codegenOCaml : Codegen
codegenOCaml = MkCG compile execute