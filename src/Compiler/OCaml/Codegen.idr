module Compiler.OCaml.Codegen

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

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
ocamlName (NS ns n) = showSep "_" ns ++ "_" ++ ocamlName n
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

comment : String -> String
comment str = fastAppend ["(* ", str, " *)\n"]

nameList : List Name -> String
nameList names = showSep " " (map ocamlName names)

mutual
    emitArg : {auto ctxt : Ref Ctxt Defs} ->
              {auto e : Ref Emitted (List String)} ->
               String -> String -> NamedCExp -> Core ()
    emitArg pfix suffix exp = do
            emit $ pfix
            compileExp exp
            emit suffix

    compileOp : {auto ctxt : Ref Ctxt Defs} ->
                {auto e : Ref Emitted (List String)} ->
                PrimFn arity -> Vect arity NamedCExp -> Core ()
    compileOp fn args = coreLift $ putStrLn ("Can't handle " ++ show fn)

    compilePrim : {auto ctxt : Ref Ctxt Defs} ->
                  {auto e : Ref Emitted (List String)} ->
                  Name -> List NamedCExp -> Core ()
    compilePrim name args = coreLift $ putStrLn ("Unknown primitive " ++ show name)

    compileExp : {auto ctxt : Ref Ctxt Defs} ->
                {auto e : Ref Emitted (List String)} ->
                NamedCExp -> Core ()
    compileExp (NmLocal fc name) = emit $ ocamlName name
    compileExp (NmRef fc name) = emit $ ocamlName name
    compileExp (NmLam fc var exp) = do
        emit $ fastAppend ["(fun ", ocamlName var, " -> "]
        compileExp exp
        emit ")"
    compileExp (NmLet fc var exp body) = do
        emit $ fastAppend ["let ", ocamlName var, " = "]
        compileExp exp
        emit " in "
        compileExp body
        emit "\n"
    compileExp (NmApp fc fn args) = do
        compileExp fn
        traverse (emitArg " (" ")") args
        emit " "
    compileExp (NmCon fc _ (Just n) args) = do
        emit $ fastAppend ["{ tag=", show n, "; vals=[ "]
        traverse (emitArg " " ";") args
        emit "] }"
    compileExp (NmCon fc name Nothing args) = do
        emit $ fastAppend ["{ name=", quotedName name, "; args=[ "]
        traverse (emitArg " " ";") args
        emit "] }"
    compileExp (NmOp fc fn args) = compileOp fn args
    compileExp (NmExtPrim fc name args) = compilePrim name args
    compileExp e = do
        coreLift $ putStrLn $ fastAppend ["I don't know how to compile ", show e]

compileFun : {auto ctxt : Ref Ctxt Defs} ->
             {auto e : Ref Emitted (List String)} ->
             Name -> FC -> List Name -> NamedCExp -> Core ()
compileFun name fc args exp = do
    emit $ fastAppend [ "let rec ", ocamlName name, " ", nameList args, " =\n" ]
    compileExp exp
    emit ";;\n\n"

compileDef' : {auto ctxt : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
             {auto e : Ref Emitted (List String)} ->
            Name -> FC -> NamedDef -> Core ()
compileDef' name fc (MkNmFun args exp) = compileFun name fc args exp
compileDef' name fc ndef = pure ()

compileDef : {auto ctxt : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
              (Name, FC, NamedDef) -> Core String
compileDef (name, fc, ndef) = do
    e <- newRef {t = List String} Emitted []
    emit $ comment (ocamlName name)
    emit $ comment (show ndef)
    compileDef' name fc ndef
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
    let ctm = forget (mainExpr cdata)

    l <- newRef {t = List String} Loaded ["libc", "libc 6"]
    s <- newRef {t = List String} Structs []
    
    pieces <- traverse compileDef ndefs
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