import Data.Char
import System.Environment
import Data.List (sort)

spelledOutDigits :: [String]
spelledOutDigits = 
    [ "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    ]

main :: IO ()
main = do
    args <- getArgs
    let part = read $ args !! 0
    case part of
        1 -> interact (show . part1 . lines)
        2 -> interact (show . part2 . lines)
        _ -> undefined

part1 :: [String] -> Int
part1 = sum . map (read . (\x -> head <$> [x, reverse x]) . filter isDigit)

part2 :: [String] -> Int
part2 = sum . map (\s -> read . (\x -> head <$> [x, reverse x]) . map snd . sort $ pickDigits s ++ matchSpelledOut s)

pickDigits :: String -> [(Int, Char)]
pickDigits = filter (isDigit . snd) . zip [0..]

matchSpelledOut :: String -> [(Int, Char)]
matchSpelledOut xs =
    foldr (++) [] . map (\(d, p) -> fmap (flip (,) d) $ match p xs 0) $ zip ['1'..'9'] spelledOutDigits
    
match :: String -> String -> Int -> [Int]
match _ [] _ = []
match pat xs pos = 
    if pat == take n xs
       then pos:rem
       else rem
    where n = length pat
          rem = match pat (tail xs) (pos+1)
