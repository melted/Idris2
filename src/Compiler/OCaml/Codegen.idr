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

nameList : List Name -> String
nameList names = showSep " " (map ocamlName names) 

compileFun : {auto ctxt : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
             Name -> FC -> List Name -> NamedCExp -> Core String
compileFun name fc args exp = do
    pure $ "let " ++ ocamlName name ++ " " ++ nameList args ++ " =\n"
                    ++ show exp ++ "\n"

compileDef : {auto ctxt : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
              (Name, FC, NamedDef) -> Core String
compileDef (name, fc, (MkNmFun args exp)) = compileFun name fc args exp
compileDef (name, fc, (MkNmCon tag arity nt)) = compileCon 
compileDef (name, fc, ndef) = pure $ "(* " ++ ocamlName name ++ " *)\n"
                                     ++ show ndef ++ "\n\n"

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
    codeDefs <- traverse compileDef ndefs
    support <- readDataFile "ocaml/support.ml"
    let code = fastAppend codeDefs
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