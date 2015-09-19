{-# LANGUAGE OverloadedStrings #-}

import CGen
import AST

test = do
    include "<stdio.h>"
    func int "times2" [(int, "x")]
    body [ declare int "y" (Just (2 * "x"))
         , returnC "y"
         ]
    func int "main" []
    body [ call "printf" [str "%d\n", call "times2" [1]]
         , call "printf" [str "%d\n", call "times2" [2]]
         , returnC 0
         ]

main = putStrLn . pretty $ runCGen test

