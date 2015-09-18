module CGen where

import AST

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

