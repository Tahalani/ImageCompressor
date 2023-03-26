{--
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- image compressor
--}
    
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GetPixel (init_pixel,
                default_pixel,
                display_pixel,
                Pixel (..)) where
import GetFlag

data Pixel = Pixel {
    pos::(Int, Int),
    pix::(Int, Int, Int)
} deriving (Eq, Show)

check_pixel :: Int -> String -> Bool
check_pixel count [] | count == 2 = True
                     | otherwise = False
check_pixel count (x:xs) | x == '(' = check_pixel (count + 1) xs
                         | otherwise = check_pixel count xs

getPixel :: [String] -> Pixel
getPixel [coord, pixel] = Pixel {pos = read coord, pix = read pixel}

pars_file :: String -> [Pixel]
pars_file str | check_pixel 0 str = [getPixel (words str)]
              | otherwise = []

init_pixel :: Param -> [Pixel] -> [String] -> [Pixel]
init_pixel param pixel [] = pixel
init_pixel param pixel (x:xs) = init_pixel param (pixel ++ pars_file x) xs

default_pixel :: [Pixel]
default_pixel = []

display_pixel :: [Pixel] -> IO ()
display_pixel = foldr ((>>) . print) (putStrLn "END\n")
