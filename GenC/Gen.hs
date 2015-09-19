{-# LANGUAGE OverloadedStrings #-}

module GenC.Gen
  ( char, short, int, long, longlong, signed, unsigned, float, double, Gen
  , GenC.Gen.void, ptr, array, GenC.Gen.return, str, ident, call, body
  , assign, declare, while, doWhile, ifC, runGen, setScope, func, include
  ) where

import GenC.AST

import Data.Maybe (fromMaybe)
import Control.Arrow (second)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Debug.Trace

unwrapIntVariant :: Type -> IntVariant
unwrapIntVariant (Integer x) = x
unwrapIntVarient _           = error "not an integer"

char = Integer Char
short = Integer Short
int = Integer Int
long = Integer Long
longlong = Integer LongLong
signed = Signed . unwrapIntVariant
unsigned = Unsigned . unwrapIntVariant
float = Floating Float
double = Floating Double
void = Void
ptr = Pointer
array = Array
return = Return
str = Literal . StringLit
ident = Literal . Identifier
call = Call
assign = Assign
declare = Declare
while = While
doWhile = DoWhile

ifC :: [(Expr, [Expr])] -> Maybe [Expr] -> Expr
ifC xs els = If (map (uncurry IfClause) xs) els

data FnData = FnData { fnRet :: Type 
                     , fnName :: Name 
                     , fnArgs :: [(Type, Name)] 
                     , fnBody :: [Expr]
                     } deriving (Eq, Show)

data GenState = GenState
               { currentFunc :: FnData
               , functions :: M.Map Name FnData
               , file :: [TopLevel]
               } deriving (Eq, Show)

type Gen a = State GenState a

runGen :: Gen a -> File
runGen cg = prepare $ execState (cg >> putCurrentFunc) defaultState
  where defaultState                = GenState cMain M.empty []
        cMain                       = FnData int "main" [(int, "argc"), (ptr (ptr char), "argv")] []
        prepare (GenState _ fns f) = f ++ (declFuncs . map toFunc . map snd . M.toList) fns 
        toFunc (FnData a b c d)     = Func a b c d
        declFuncs = declFuncs' [] []
        declFuncs' zs ys [] = ys ++ zs
        declFuncs' zs ys ((Func ret name args body):xs) =
          declFuncs' ((Func ret name args body):zs)
                     ((FuncDecl ret name (map (second (const Nothing)) args)):ys)
                     xs

putCurrentFunc :: Gen ()
putCurrentFunc = do
    funcs <- gets functions
    cf <- gets currentFunc
    let newCf = cf { fnBody = reverse (fnBody cf) }
    modify $ \cg -> cg { functions = M.insert (fnName cf) newCf (functions cg) }

setScope :: Name -> Gen ()
setScope name = do
    putCurrentFunc
    funcs <- gets functions
    case M.lookup name funcs of
      Just f  -> modify $ \cg -> cg { currentFunc = f }
      Nothing -> error $ "setScope: '" ++ name ++ "' is not in scope."

func :: Type -> Name -> [(Type, Name)] -> Gen Name
func ret name args = do
    putCurrentFunc
    modify $ \cg -> cg { currentFunc = FnData ret name args [] }
    Prelude.return name

include :: Name -> Gen ()
include n = modify $ \cg -> cg { file = (Directive (Include n)) : file cg }

body :: [Expr] -> Gen ()
body exp = do
    cf <- gets currentFunc
    let newCf = cf { fnBody = reverse exp ++ fnBody cf }
    modify $ \cg -> cg { currentFunc = newCf }

