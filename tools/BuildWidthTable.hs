module Main (main) where

import Graphics.Vty.UnicodeWidthTable.Main (defaultMain)
import System.Console.ANSI (getCursorPosition)

main :: IO ()
main = defaultMain charWidth

-- | The number of times we'll attempt to compute a character's width
-- before defaulting to @1@ and continuing.
attempts :: Int
attempts = 3

-- | Experimentally determine the console width of the character.
charWidth :: Char -> IO Int
charWidth = charWidth' 0

-- | Helper for 'charWidth' with the number of failed attempts counted.
charWidth' :: Int {- ^ failed attempts -} -> Char -> IO Int
charWidth' n c
    | n >= attempts =
     do putStrLn ("\rUnable to check: " ++ [c] ++ " (" ++ show c ++ ")")
        pure 1 -- fallback default to 1
    | otherwise =
     do putStr ['\r', c]
        mb <- getCursorPosition
        case mb of
            Nothing -> charWidth' (n+1) c
            Just (_, col) -> pure col
