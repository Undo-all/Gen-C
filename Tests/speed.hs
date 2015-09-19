{-# LANGUAGE OverloadedStrings #-}

import CGen
import AST

test = do
    func int "main" []
    body [declare int "x"]
    body [assign "x" 0]
    body $ replicate 10000 (assign "x" ("x" + 1))

main = do
    putStrLn . pretty $ runCGen test

