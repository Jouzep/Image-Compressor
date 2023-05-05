{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-joseph.yu
-- File description:
-- Lib
-}

module Lib
    (
    parseArgs,
    defaultConf,
    checkFile,
    Conf
    ) where

import Tools
import Data.Maybe
import System.Exit
import System.Random
import System.IO
import ErrorHandling
import Data.Maybe (fromJust)
import Data.List (nub)

type Point = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Point, Color)

data Conf = Conf {
    color :: Maybe Int,
    convergence :: Maybe Float,
    file :: Maybe String
} deriving (Show, Eq)

data Cluster = Cluster {
    cluster :: Maybe Color,
    pixel :: [Pixel]
} deriving (Show, Eq)

defaultCluster :: Cluster
defaultCluster = Cluster {
    cluster = Nothing,
    pixel = []
}

defaultConf :: Conf
defaultConf = Conf {
    color = Nothing,
    convergence = Nothing,
    file = Nothing
}

parseArgs :: Conf -> [String] -> Maybe Conf
parseArgs (Conf _ convergence file) ("-n":x:xs) =
    if (readIntNeg x == Nothing)
        then Nothing
    else parseArgs (Conf (readInt x) convergence file) xs
parseArgs (Conf color _ file) ("-l":x:xs) =
    if (readFloatNeg x == Nothing)
        then Nothing
    else parseArgs (Conf color (readFloat x) file) xs
parseArgs (Conf color convergence _) ("-f":x:xs) =
    parseArgs (Conf color convergence (Just x)) xs
parseArgs (Conf Nothing _ _) _ = Nothing
parseArgs (Conf _ Nothing _) _ = Nothing
parseArgs (Conf _ _ Nothing) _ = Nothing
parseArgs conf [] = Just conf
parseArgs _ _ = Nothing

checkFile :: Conf -> IO ()
checkFile (Conf a b c) = do
    file <- tryReadFile (fromJust(c))
    case file of
        False -> (exitWithMsg "Invalid File" (ExitFailure 84))
        True -> parseFile (Conf a b c)

fileContent :: FilePath -> IO String
fileContent filePath = readFile filePath

parseFile :: Conf -> IO ()
parseFile (Conf a b (Just c)) =
    fileContent c >>= \content ->
    doCompressor (Conf a b (Just c)) (words content)

fillData :: [String] -> [Pixel]
fillData [] = []
fillData (a:b:c) = [((getPoint a), (getColor b))] ++ (fillData c)

getPoint :: String -> Point
getPoint str = read str :: (Int, Int)

getColor :: String -> Color
getColor str = read str :: (Int, Int, Int)

-- ______________ GETTER ______________

getClusterColor :: Cluster -> Color
getClusterColor (Cluster c p) = (fromJust c)

csColor :: [Cluster] -> [Color]
csColor [] = []
csColor ((Cluster c p):next) = [(fromJust c)] ++ csColor next

clustPixCol :: Cluster -> [Color]
clustPixCol (Cluster c p) = getPixsColor p

getPixsColor :: [Pixel] -> [Color]
getPixsColor [] = []
getPixsColor ((p, c):ps) = [c] ++ getPixsColor ps

getPixColor :: Pixel -> Color
getPixColor (p, c) = c

getListPixel :: [Color] -> [Pixel] -> Color -> [Pixel]
getListPixel _ [] _ = []
getListPixel colList (pix:pixs) col =
    let closest = (findClosest colList (getPixColor pix) col 100000)
    in if (closest == col)
        then [pix] ++ getListPixel colList pixs col
        else getListPixel colList pixs col

-- ______________ UTILS _______________

sqr :: Int -> Int
sqr x = x * x

checkExist :: Int -> [Int] -> Bool
checkExist _ [] = False
checkExist x (i:is) =
    if (x == i)
        then True
    else checkExist x is

randomIntList :: Int -> Int -> [Int] -> IO [Int]
randomIntList 0 _ res = return res
randomIntList n range res = do
    rand <- randomRIO (0, range)
    if ((checkExist rand res) == True)
        then randomIntList n range res
    else randomIntList (n - 1) range (res ++ [rand])

calcDistance :: Color -> Color -> Float
calcDistance (a, b, c) (d, e, f) =
    sqrt(fromIntegral (sqr(a - d) + sqr(b - e) + sqr(c - f)))

removeX :: [Cluster] -> Int -> [Cluster]
removeX (x:xs) index =
    if (index == 0)
        then xs
    else removeX xs (index - 1)

displayPixel :: [Pixel] -> IO ()
displayPixel [] = return ()
displayPixel ((p,c):ps) =
    putStrLn (show (p) ++ " " ++ show (c)) >> displayPixel ps

displayCluster :: [Cluster] -> Int -> IO ()
displayCluster [] _ = return ()
displayCluster clusters 0 =
    putStrLn "--" >> displayCluster clusters 1
displayCluster ((Cluster c pix):next) 1 =
    putStrLn (show (fromJust c)) >> displayCluster ((Cluster c pix):next) 2
displayCluster clusters 2 =
    putStrLn "-" >> displayCluster clusters 3
displayCluster ((Cluster c pix):next) 3 =
    displayPixel pix >> displayCluster next 0

addColor :: Color -> Color -> Color
addColor (a, b, c) (d, e, f) = (a + d, b + e, c + f)

calcAverage :: [Color] -> Int -> Color -> Color
calcAverage [] index (a, b, c) = ((div a index), (div b index), (div c index))
calcAverage (c:cs) index res = calcAverage cs (index + 1) (addColor res c)

findClosest :: [Color] -> Color -> Color -> Float -> Color
findClosest [] _ cmp _ = cmp
findClosest (c:cs) pixel cmp limit =
    let res = (calcDistance c pixel)
    in if (res < limit)
        then findClosest cs pixel c res
        else findClosest cs pixel cmp limit

saveCluster :: [Int] -> [Pixel] -> [Color]
saveCluster [] pixel = []
saveCluster (x:xs) pixel = [getPixColor (pixel !! x)] ++ saveCluster xs pixel

fillCluster :: [Cluster] -> [Color] -> [Cluster]
fillCluster res [] = removeX (res) 0
fillCluster clusters (x:xs) =
    fillCluster (clusters ++ [Cluster (Just x) (pixel (last clusters))]) xs

fillPixel :: [Cluster] -> [Color] -> [Pixel] -> [Cluster]
fillPixel [] _ _ = []
fillPixel ((Cluster c p):next) col parsed =
    let list = getListPixel col parsed (fromJust c)
    in [(Cluster c list)] ++ fillPixel next col parsed

-- ________________ KMEAN _________________

clusterInit :: [Pixel] -> [Int] -> [Cluster]
clusterInit parsed randList =
    let pixList = saveCluster randList parsed
        cluster = fillCluster [defaultCluster] pixList
        color = (getClusterColor (cluster !! 1))
        clusterPixList = getListPixel pixList parsed color
    in fillPixel cluster pixList parsed

checkColor :: [Color] -> [Color] -> Float -> Bool
checkColor [] [] _ = True
checkColor (old:os) (new:ns) limit =
    if ((calcDistance old new) > limit)
        then False
        else checkColor os ns limit

calcClusterAverage :: [Cluster] -> [Cluster] -> [Cluster]
calcClusterAverage [] new = new
calcClusterAverage ((Cluster c p):cs) new =
    let average = calcAverage (getPixsColor p) 0 (0, 0, 0)
    in calcClusterAverage cs (new ++ [(Cluster (Just average) [])])

doKmeans :: [Cluster] -> [Cluster] -> [Pixel] -> Float -> [Cluster]
doKmeans old new parsed limit =
    let check = (checkColor (csColor old) (csColor new) limit)
        clust = (calcClusterAverage (fillPixel old (csColor old) parsed) [])
    in if (check == True)
        then fillPixel new (csColor new) parsed
        else doKmeans new clust parsed limit

doCompressor :: Conf -> [String] -> IO ()
doCompressor (Conf c n l) list = do
    let parsed = fillData list
    randList <- (randomIntList (fromJust c) ((length parsed) - 1) [])
    let cluster = clusterInit parsed (randList)
    let newCluster = (calcClusterAverage cluster [])
    let res = doKmeans cluster newCluster parsed (fromJust n)
    -- res = calcClusterAverage cluster
    displayCluster (res) 0