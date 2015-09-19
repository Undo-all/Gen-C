{-# LANGUAGE OverloadedStrings #-}

import CGen
import AST

test = do
    include "<stdio.h>"
    func int "times2" [(int, "x")]
    body [returnC $ 2 * "x"]
    func int "main" []
    body [ call "printf" [str "%d\n", call "times2" [1]]
         , call "printf" [str "%d\n", call "times2" [2]]
         , returnC 0
         ]

main = either putStrLn (putStrLn . pretty) $ runCGen test

