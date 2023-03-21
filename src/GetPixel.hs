{--
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- image compressor
--}

module GetPixel (init_pixel, default_pixel, Pixel (..)) where
import GetFlag

data Pixel = Pixel {
    pos::(Int, Int),
    pix::(Int, Int, Int)
} deriving (Eq, Show)

init_pixel :: Param -> Pixel -> Pixel
init_pixel _ pixel = pixel

default_pixel :: Pixel
default_pixel = Pixel {pos = (0, 0), pix = (0, 0, 0)}
