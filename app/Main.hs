{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-joseph.yu
-- File description:
-- Main
-}

module Main (main) where

import System.Environment
import System.Exit
import Lib
import Tools
import ErrorHandling

main :: IO ()
main = do
    arg <- getArgs
    let a = parseArgs defaultConf arg
    case a of
        Just a -> checkFile a
        Nothing -> (exitWithMsg "Error Args" (ExitFailure 84))