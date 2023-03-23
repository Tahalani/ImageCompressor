{--
-- EPITECH PROJECT, 2023
-- algo
-- File description:
-- image compressor
--}

module Algo (calculDistance, createRandomCluster, Param (..)) where
import GetFlag
import GetPixel
import System.Random

data Cluster = Cluster {
    colors::(Int, Int, Int),
    coord::[Pixel]
} deriving(Show)

mysqrt :: Float -> Float
mysqrt x = x ** 0.5

sqr :: Int -> Int
sqr i = i * i

calculDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
calculDistance (r1, g1, b1) (r2, g2, b2) = mysqrt (fromIntegral (sqr (r1 - r2) + sqr (g1 - g2) + sqr (b1 - b2)))

getRandomCluster :: [Pixel] -> IO (Int, Int, Int)
getRandomCluster pixelArray = do
    rand <- randomRIO (0, length pixelArray - 1)
    return (pix (pixelArray !! rand))

createRandomCluster :: Int -> [Cluster] -> [Pixel] -> IO [Cluster]
createRandomCluster 0 randCluster _ = return (randCluster)
createRandomCluster n randCluster pixe = do
    rand <- getRandomCluster(pixe)
    createRandomCluster (n - 1) (randCluster++[Cluster{colors = rand, coord = []}]) pixe
