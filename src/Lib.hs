{--
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- image compressor
--}

module Lib (my_image_compressor) where

import System.Environment (getArgs)
import System.Exit ( exitSuccess )
import GetFlag
import GetPixel

my_image_compressor :: IO ()
my_image_compressor = do
    args <- getArgs
    let param = init_param args default_param
    let pixel = init_pixel param default_pixel
    display_param param
    exitSuccess
