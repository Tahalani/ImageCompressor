{--
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- image compressor
--}

module GetPixel (init_pixel,
                default_pixel,
                Pixel (..)) where
import GetFlag

data Pixel = Pixel {
    pos::(Int, Int),
    pix::(Int, Int, Int)
} deriving (Eq, Show)

check_pixel :: Int -> String -> Bool
check_pixel 2 [] = True
check_pixel _ [] = False
check_pixel count ('(':xs) = check_pixel (count + 1) xs
check_pixel count (_:xs) = check_pixel count xs

getPixel :: [String] -> Pixel
getPixel [coord, pixel] = Pixel {pos = read coord, pix = read pixel}
getPixel _ = Pixel {pos = (0, 0), pix = (0, 0, 0)}

pars_file :: String -> [Pixel]
pars_file str | check_pixel 0 str = [getPixel (words str)]
              | otherwise = []

init_pixel :: Param -> [Pixel] -> [String] -> [Pixel]
init_pixel _ pixel [] = pixel
init_pixel param pixel (x:xs) = init_pixel param (pixel ++ pars_file x) xs

default_pixel :: [Pixel]
default_pixel = []
