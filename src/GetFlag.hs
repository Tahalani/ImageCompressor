{--
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- image compressor
--}

module GetFlag (display_param, default_param, init_param, Param (..)) where

data Param = Param {
    number :: Int,
    limit :: Float,
    file :: String
} deriving (Eq, Show)

init_param :: [String] -> Param -> Param
init_param [] param = param
init_param ("-n":sec:list) (Param _ l f) =
    init_param list (Param (read sec :: Int ) l f)
init_param ("-l":sec:list) (Param n _ f) =
    init_param list (Param n (read sec :: Float ) f)
init_param ("-f":sec:list) (Param n l _) =
    init_param list (Param n l sec)
init_param (_:_:_) (Param n l f) = (Param n l f)

default_param :: Param
default_param = Param {number = 0, limit = 0, file = "test.txt"}

display_param :: Param -> IO ()
display_param param = do
    putStrLn $ "number: " ++ (show $ number param)
    putStrLn $ "limit: " ++ show (limit param)
    putStrLn $ "file: " ++ file param
