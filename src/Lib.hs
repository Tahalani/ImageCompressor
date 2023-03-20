module Lib (my_image_compressor) where

import System.Environment (getArgs)
import System.Exit

calculDistance :: [String] -> IO ()
calculDistance [] = return ()
calculDistance a b = return ()

my_image_compressor :: IO ()
my_image_compressor = do
    args <- getArgs
    calculDistance args
    exitSuccess
