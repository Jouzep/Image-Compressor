{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-joseph.yu
-- File description:
-- ErrorHandling
-}

module ErrorHandling
    (
        exitWithMsg,
        tryReadFile,
    ) where

-- import Tools
import System.Environment
import System.Exit
import System.IO
import Control.Exception (try, SomeException)
exitWithMsg :: String -> ExitCode -> IO ()
exitWithMsg str code = hPutStrLn stderr str >> exitWith code

short :: Int -> Int
short x
    | x < 0 = -1
    | x > 256 =  -1
    | otherwise = x


tryReadFile :: String -> IO Bool
tryReadFile file = do
    result <- (try $ readFile file) :: IO (Either SomeException String)
    return $ case result of
        Left _ -> False
        Right _ -> True

