{--
-- EPITECH PROJECT, 2023
-- algo
-- File description:
-- image compressor
--}

module Algo (createRandomCluster,
            replaceCluster,
            mainLoop,
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
    return (pix (pixelArray !! rand))

createRandomCluster :: Int -> [Cluster] -> [Pixel] -> IO [Cluster]
createRandomCluster 0 randCluster _ = return (randCluster)
createRandomCluster n randc p = do
    rand <- getRandomCluster(p)
    createRandomCluster (n - 1) (randc++[Cluster{colors = rand, coord = []}]) p


linkCluster :: [Cluster] -> Pixel -> Float -> [Cluster]
linkCluster [] _ _ = []
linkCluster (cluster:clusters) pixel mindistance =
    if (calculDistance (colors cluster) (pix pixel)) == mindistance
    then (Cluster{colors = colors cluster, coord
    = (coord cluster) ++ [pixel]}) : clusters
    else cluster : linkCluster clusters pixel mindistance

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

averageColor :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
averageColor [] (r, g, b) = (r, g, b)
averageColor ((r, g, b):c) (r1, g1, b1) =
    averageColor c (r + r1, g + g1, b + b1)

divisionAverage :: (Int, Int, Int) -> Int -> (Int, Int, Int)
divisionAverage (r, g, b) 0 = (r, g, b)
divisionAverage (r, g, b) n = (r `div` n, g `div` n, b `div` n)

handleColorCluster :: Cluster -> (Int, Int, Int)
handleColorCluster cluster = if (length (coord cluster)) == 0
    then colors cluster
    else divisionAverage (averageColor (map pix (coord cluster))
        (0, 0, 0)) (length (coord cluster))

replaceCluster :: [Cluster] -> [Cluster]
replaceCluster [] = []
replaceCluster (clust:clusters) = clust{colors = handleColorCluster clust,
    coord = (coord clust)} : replaceCluster clusters

printPixels :: [Pixel] -> IO()
printPixels [] = return ()
printPixels (res:nextPix) = putStr(show (pos res)) >> putStr " "
    >> putStrLn(show (pix res)) >> printPixels nextPix

printResult :: [Cluster] -> IO()
printResult [] = return ()
printResult (res:nextPix) = putStrLn("--") >> print (colors res)
    >> putStrLn("-") >> printPixels (coord res) >> printResult nextPix

checkLim :: [Cluster] -> [Cluster] -> Float -> Bool
checkLim [] [] _ = True
checkLim (a:as) (b:bs) lim
        | ((calculDistance (colors a) (colors b)) > lim) = False
        | otherwise = checkLim as bs lim

mainLoop :: [Cluster] -> [Cluster] -> [Pixel] -> Float -> IO ()
mainLoop old new list l |
    ((checkLim old new l) == True) = printResult (addToCluster new list)
    | otherwise = mainLoop new (replaceCluster (addToCluster new list)) list l
