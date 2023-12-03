{-# LANGUAGE TupleSections #-}

import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.Char
import System.Environment
import Data.List (sort)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.Vector qualified as V
import Data.Bifunctor (bimap, first)
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap qualified as IntMap

main :: IO ()
main = do
    args <- getArgs
    let part = read $ head args
    case part of
        1 -> BS.interact (BS.pack . show . part1 . BS.lines)
        2 -> BS.interact (BS.pack . show . part2 . BS.lines)
        _ -> undefined

neighbors :: [(Int, Int)]
neighbors = do
    dx <- [-1, 0, 1]
    dy <- [-1, 0, 1]
    guard $ (dx, dy) /= (0,0)
    return (dx,dy)

part1 :: [ByteString] -> Int
part1 lines = 
    let n = length lines
        m = BS.length $ head lines
        inRange r c = 0 <= r && r < n && 0 <= c && c < m
        flattened = V.fromList . BS.unpack . BS.concat $ lines
        isSymbol r c = (\w -> not (isDigit w) && (w/='.')) $ flattened V.! (r*m + c)
        grid = chunksOf m . V.toList $ V.imap (\p a -> (a, Any . uncurry (mark inRange $ any (uncurry isSymbol)) $ toRowCol m p)) flattened
    in sum $ concatMap (map ((read :: (String -> Int)) . fst) . filter (getAny . snd) . clumpDigits mempty) grid

part2 :: [ByteString] -> Integer
part2 lines = 
    let n = length lines
        m = BS.length $ head lines
        inRange r c = 0 <= r && r < n && 0 <= c && c < m
        flattened = V.fromList . zip [(0::Int)..] . BS.unpack . BS.concat $ lines
        maybeGear r c = (\(i,v) -> i <$ guard (v =='*')) $ flattened V.! (r*m + c)
        grid = chunksOf m . V.toList $ V.imap (\p a -> (a, uncurry (mark inRange . mapMaybe $ uncurry maybeGear) $ toRowCol m p)) $ V.map snd flattened
        numToGears = concatMap (filter (not . null . snd) . fmap (bimap (read :: String -> Integer) nubOrd) . clumpDigits mempty) grid
        gearToNums = IntMap.filter ((==2) . length) . IntMap.fromListWith (<>) $ concatMap (\(d, l) -> (,[d]) <$> l) numToGears
    in sum . fmap (getProduct . foldr ((<>) . Product) 1)  $ IntMap.elems gearToNums


clumpDigits :: Monoid b => (String, b) -> [(Char, b)] -> [(String, b)]
clumpDigits (s, b) [] = [(reverse s, b) | not (null s)]
clumpDigits (s, b) ((x,b'):xs) 
    | isDigit x = clumpDigits (x:s, b <> b') xs
    | null s = clumpDigits mempty xs
    | otherwise = (reverse s, b):clumpDigits mempty xs

toRowCol :: Int -> Int -> (Int, Int)
toRowCol m p = (p `div` m, p `mod` m)

mark :: (Int -> Int -> Bool) -> ([(Int, Int)] -> a) -> Int -> Int -> a
mark chooseNeighbors applyToNeighbors r c = applyToNeighbors . filter (uncurry chooseNeighbors) . fmap (bimap (+r) (+c)) $ neighbors

chunksOf :: Int -> [a] -> [[a]]
chunksOf m [] = []
chunksOf m l = take m l : chunksOf m (drop m l)

