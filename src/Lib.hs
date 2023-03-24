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
import Algo

my_image_compressor :: IO ()
my_image_compressor = do
    args <- getArgs
    let param = init_param args default_param
    buffer <- readFile (file param)
    let pixel = init_pixel param default_pixel (lines buffer)
    cluster <- createRandomCluster (number param) [] pixel
    mainLoop cluster (replaceCluster cluster) pixel (limit param)
    exitSuccess
