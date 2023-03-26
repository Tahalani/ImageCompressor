{--
-- EPITECH PROJECT, 2023
-- algo
-- File description:
-- image compressor
--}

module Algo (createRandomCluster,
            mainLoop,
            averageColor,
            addToCluster,
            Param (..)) where

import GetFlag
import GetPixel
import System.Random

data Cluster = Cluster {
    colors::(Int, Int, Int),
    coord::[Pixel]
} deriving(Show)

sqr :: Int -> Int
sqr i = i * i

calculDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
calculDistance (r1, g1, b1) (r2, g2, b2) =
    sqrt (fromIntegral (sqr (r1 - r2) + sqr (g1 - g2) + sqr (b1 - b2)))

getRandomCluster :: [Pixel] -> IO (Int, Int, Int)
getRandomCluster pixelArray = do
    rand <- randomRIO (0, length pixelArray - 1)
    let (x, y, z) = pix (pixelArray !! rand)
    return (x, y, z)

createRandomCluster :: Int -> [Pixel] -> [Cluster] -> IO [Cluster]
createRandomCluster 0 _ randCluster = return (randCluster)
createRandomCluster n pixOne randc = do
    rand <- getRandomCluster(pixOne)
    createRandomCluster (n - 1) pixOne
        (randc ++ [Cluster{colors = rand, coord = []}])

linkCluster :: [Cluster] -> Pixel -> Float -> [Cluster]
linkCluster [] _ _ = []
linkCluster (cluster:clusters) pixel mindistance | (mindistance ==
    (calculDistance (colors cluster) (pix pixel))) = (Cluster{colors =
    colors cluster, coord = (coord cluster) ++ [pixel]}) : clusters
    | otherwise = cluster : linkCluster clusters pixel mindistance

funcAverage :: [Pixel] -> Int -> (Int, Int, Int) -> (Int, Int, Int)
funcAverage [] 0 (x, y, z) = (x, y, z)
funcAverage [] i (x, y, z) = ((x `div` i), (y `div` i), (z `div` i))
funcAverage (s:xs) i (a, b, c) =
    funcAverage xs (i + 1) (x + a, y + b, z + c) where (x, y, z) = (pix s)

averageColor :: [Cluster] -> [Cluster] ->[Cluster]
averageColor [] newClust = newClust
averageColor (clust:clusternext) newClust
    = averageColor clusternext (newClust++[(clust
    {colors = funcAverage(coord clust) 0 (0, 0, 0)} {coord = []})])

calculMinDistance :: [Cluster] -> Pixel -> Float
calculMinDistance [] _ = 100
calculMinDistance (cluster:[]) pixel =
    calculDistance (colors cluster) (pix pixel)
calculMinDistance (cluster:clusters) pixel = min (calculDistance
    (colors cluster) (pix pixel)) (calculMinDistance clusters pixel)

addToCluster :: [Cluster] -> [Pixel] -> [Cluster]
addToCluster [] _ = []
addToCluster (cluster:clusters) [] = cluster : clusters
addToCluster cluster (pixelone:pixel) = addToCluster
    (linkCluster cluster pixelone (calculMinDistance cluster pixelone)) pixel

printPixels :: [Pixel] -> IO()
printPixels [] = return ()
printPixels (res:rest) = putStr(show (pos res)) >> putStr " "
    >> putStrLn(show (pix res)) >> printPixels rest

printFinalRes :: [Cluster] -> IO()
printFinalRes [] = return ()
printFinalRes (res:rest) = putStrLn("--") >> print (colors res)
    >> putStrLn("-") >> printPixels (coord res) >> printFinalRes rest

checkLim :: [Cluster] -> [Cluster] -> Float -> Bool
checkLim [] [] _ = True
checkLim (a:as) (b:bs) lim
        | ((calculDistance (colors a) (colors b)) > lim) = False
        | otherwise = checkLim as bs lim
checkLim _ _ _ = False

mainLoop :: [Cluster] -> [Cluster] -> [Pixel] -> Float -> IO ()
mainLoop old new list lim |
    ((checkLim old new lim) == False) =
        mainLoop new (averageColor (addToCluster new list) []) list lim
    | otherwise = printFinalRes (addToCluster new list)
