{--
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- image compressor
--}

module ErrorHandling (handle_args) where
import System.Exit

checkDigits :: [Char] -> [Char] -> Bool
checkDigits _ [] = True
checkDigits "-f" (_:xs) = checkDigits "-f" xs
checkDigits flags ('-':xs) = checkDigits flags xs
checkDigits flags ('.':xs) = checkDigits flags xs
checkDigits flags (x:xs) | x < '0' || x > '9' = False
                   | otherwise = checkDigits flags xs

checkBuffer :: [String] -> Bool
checkBuffer [] = True
checkBuffer (x:xs) | elem x ["-n", "-l", "-f"] == True
                    = checkBuffer xs
                   | otherwise = False

handle_args :: [String] -> IO ()
handle_args [] = return ();
handle_args (a:_:_)  | checkBuffer [a] == False =
            putStrLn "Error: Bad flags" >> exitWith(ExitFailure 84)
handle_args (a:b:xs) | checkDigits a b == False =
            putStrLn "Error: Bad args" >> exitWith(ExitFailure 84)
                     | otherwise = handle_args xs
handle_args (_:_) = putStrLn "Error: Bad flags" >> exitWith(ExitFailure 84)