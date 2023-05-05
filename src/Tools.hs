{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-joseph.yu
-- File description:
-- Tools
-}

module Tools
(
    readInt,
    readIntNeg,
    readFloat,
    readFloatNeg,
) where

import Data.Char

checkFloat :: String -> Float -> Bool
checkFloat (x:xs) idx = if isDigit x || (x == '.' || (x == '-' && idx == 0))
    then checkFloat xs (idx + 1)
    else False
checkFloat [] _ = True

readFloat :: String -> Maybe Float
readFloat [] = Nothing
readFloat x
    | (checkFloat x 0) == False = Nothing
    | otherwise = Just (read x)

readFloatNeg :: String -> Maybe Float
readFloatNeg [] = Nothing
readFloatNeg x
    | (checkFloat x 1) == False = Nothing
    | otherwise = Just (read x)

checkInt :: [Char] -> Int -> Bool
checkInt (x:xs) idx = if isDigit x || (x == '-' && idx == 0)
    then checkInt xs (idx + 1)
    else False
checkInt [] _ = True

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt x
    | (checkInt x 0) == False = Nothing
    | otherwise = Just (read x)

readIntNeg :: [Char] -> Maybe Int
readIntNeg [] = Nothing
readIntNeg x
    | (checkInt x 1) == False = Nothing
    | otherwise = Just (read x)