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

module GetPixel (init_pixel, default_pixel, display_pixel, Pixel (..)) where
import GetFlag

data Pixel = Pixel {
    pos::[(Int, Int)],
    pix::[(Int, Int, Int)]
} deriving (Eq, Show)

pars_file :: [Pixel] -> String -> [Pixel]
pars_file pixel [] = pixel
pars_file pixel (x:xs) = [Pixel {pos = [(1, 2)], pix = [(3, 4, 5)]}]

init_pixel :: Param -> [Pixel] -> [String] -> [Pixel]
init_pixel param pixel [] = pixel
init_pixel param pixel (x:xs) = init_pixel param (pixel ++ pars_file pixel x) xs

-- display_array :: [String] -> IO ()
-- display_array [] = putStrLn "END\n";
-- display_array (x:xs) = putStrLn x >> display_array xs

default_pixel :: [Pixel]
default_pixel = []

display_pixel :: [Pixel] -> IO ()
display_pixel [] = putStrLn "END\n";
display_pixel (x:xs) = putStrLn (show x) >> display_pixel xs