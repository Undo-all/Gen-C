{-# LANGUAGE FlexibleInstances,
             OverloadedStrings  #-}

module AST where

import Data.List (intercalate)
import Data.String

indent :: Int -> Name
indent n = replicate (4*n) ' '

unlist :: [Name] -> Name
unlist = intercalate ", "

type Name = String

class Pretty a where
    indentPretty :: Int -> a -> String
    pretty :: a -> String
    pretty = indentPretty 0

type File = [TopLevel]

instance Pretty File where
    indentPretty n = unlines . map (indentPretty n) 

data TopLevel = Directive Directive
              | Global Type Name (Maybe Constant)
              | FuncDecl Type Name [(Type, Maybe Name)]
              | Func Type Name [(Type, Name)] [Expr]
              deriving (Eq, Show)

instance Pretty (Type, Name) where
    indentPretty i (t, n) = indentPretty i t ++ " " ++ n

instance Pretty (Type, Maybe Name) where
    indentPretty i (t, Just n)  = indentPretty i t ++ " " ++ n
    indentPretty i (t, Nothing) = indentPretty i t 

instance Pretty TopLevel where
    indentPretty i (Directive d)         = indentPretty i d
    indentPretty i (Global t n Nothing)  = indentPretty i t ++ " " ++ n ++ ";"
    indentPretty i (Global t n (Just v)) = indentPretty i t ++ " " ++ n ++ " = " ++ pretty v ++ ";"
    indentPretty i (FuncDecl ret name args) =
      indentPretty i ret ++ " " ++ name ++ "(" ++ unlist (map pretty args) ++ ");"
    indentPretty i (Func ret name args body) =
      "\n" ++ indentPretty i ret ++ " " ++ name ++ "(" ++ unlist (map pretty args) ++ ") {\n" ++
      intercalate ";\n" (map (indentPretty (i+1)) body) ++ ";\n" ++ indent i ++ "}"

data Directive = Include Name
               | Pragma Name
               deriving (Eq, Show)

instance Pretty Directive where
    indentPretty i (Include s) = indent i ++ "#include " ++ s
    indentPretty i (Pragma s)  = indent i ++ "#pragma " ++ s

data Type = Integer IntVariant
          | Floating FloatVariant
          | Pointer Type
          | Array Type [Maybe Int]
          | FuncPtr Type [Type]
          | Struct Name
          | Union Name
          | Enum Name
          | Void
          deriving (Eq, Show)

instance Pretty Type where
    indentPretty i (Integer var)  = indentPretty i var
    indentPretty i (Floating var) = indentPretty i var
    indentPretty i (Pointer t)    = indentPretty i t ++ "*"
    indentPretty i (Array t xs)   = indentPretty i t ++ concat (map showArr xs)
      where showArr (Just n) = "[" ++ show n ++ "]"
            showArr Nothing  = "[]"
    indentPretty i (Struct s)     = indent i ++ "struct " ++ s
    indentPretty i (Union s)      = indent i ++ "union " ++ s
    indentPretty i (Enum s)       = indent i ++ "enum " ++ s
    indentPretty i (FuncPtr ret args) =
      indentPretty i ret ++ " (*)(" ++ unlist (map pretty args) ++ ")"

data IntVariant = Char
                | Short
                | Int
                | Long
                | LongLong
                | Signed IntVariant
                | Unsigned IntVariant
                deriving (Eq, Show)

instance Pretty IntVariant where
    indentPretty i Char         = indent i ++ "char"
    indentPretty i Short        = indent i ++ "short"
    indentPretty i Int          = indent i ++ "int"
    indentPretty i Long         = indent i ++ "long"
    indentPretty i LongLong     = indent i ++ "long long"
    indentPretty i (Signed t)   = indent i ++ "signed " ++ pretty t
    indentPretty i (Unsigned t) = indent i ++ "unsigned " ++ pretty t

data FloatVariant = Float
                  | Double
                  | LongDouble
                  deriving (Eq, Show)

instance Pretty FloatVariant where
    indentPretty i Float      = indent i ++ "float"
    indentPretty i Double     = indent i ++ "double"
    indentPretty i LongDouble = indent i ++ "long double"

data Constant = Identifier Name
              | IntegerLit Int
              | FloatingLit Double
              | StringLit String
              deriving (Eq, Show)

instance Pretty Constant where
    indentPretty i (Identifier n)  = indent i ++ n
    indentPretty i (IntegerLit x)  = indent i ++ show x
    indentPretty i (FloatingLit x) = indent i ++ show x
    indentPretty i (StringLit s)   = indent i ++ show s

data Expr = Call Name [Expr]
          | BinOp Op Expr Expr
          | Return Expr
          | Literal Constant
          deriving (Eq, Show)

instance Num Expr where
    x + y = BinOp Add x y
    x - y = BinOp Sub x y
    x * y = BinOp Mul x y
    fromInteger n = Literal (IntegerLit (fromInteger n))
    abs x = Call "abs" [x]
    signum x = BinOp Sub (BinOp GR x (Literal (IntegerLit 0))) (BinOp LS x (Literal (IntegerLit 0)))

instance Fractional Expr where
    x / y = BinOp Div x y
    fromRational n = Literal (FloatingLit (fromRational n))

instance IsString Expr where
    fromString s = Literal (Identifier s)

data Op = Add
        | Sub
        | Mul
        | Div
        | LS
        | GR
        deriving (Eq, Show)

instance Pretty Op where
    indentPretty i Add = indent i ++ "+"
    indentPretty i Sub = indent i ++ "-"
    indentPretty i Mul = indent i ++ "*"
    indentPretty i Div = indent i ++ "/"
    indentPretty i LS  = indent i ++ "<"
    indentPretty i GR  = indent i ++ ">"

instance Pretty Expr where
    indentPretty i (Call n args)  = n ++ "(" ++ unlist (map pretty args) ++ ")"
    indentPretty i (BinOp op x y) = indent i ++ "(" ++ pretty x ++ ") " ++ pretty op ++ " (" ++ pretty y ++ ")"
    indentPretty i (Return exp)   = indent i ++ "return " ++ pretty exp
    indentPretty i (Literal val)  = indentPretty i val

