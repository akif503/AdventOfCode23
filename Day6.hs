import Control.Applicative
import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.Char
import System.Environment
import Data.List (sort, foldl')
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.Vector qualified as V
import Data.Bifunctor (bimap, first)
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)

main :: IO ()
main = do
    args <- getArgs
    let part = read $ head args
    case part of
        1 -> BS.interact (BS.pack . show . part1 . parse . BS.lines)
        2 -> BS.interact (BS.pack . show . part2 . parse . BS.lines)
        _ -> undefined

data Race = Race Int Int
    deriving (Show, Eq)

parse :: [ByteString] -> [Race]
parse [line1, line2] = 
    let f = fmap (read . BS.unpack) . filter (not . BS.null) . BS.words . BS.drop 1 . BS.dropWhile (/=':')
    in uncurry Race <$> zip (f line1) (f line2)

-- TODO: Do binary search from [mid:t+1] (find the first False) and [0:mid] (find the first True)
-- Since, they are monotonic.
noOfWinningTimes :: Int -> Int -> Int
noOfWinningTimes t d = length . filter id . fmap ((>d) . uncurry (*)) . take (t+1) . zip (iterate (+(-1)) t) $ iterate (+1) 0

firstWinningTime :: Int -> Int -> Int
firstWinningTime t d = length . takeWhile not . fmap ((>d) . uncurry (*)) . take (t+1) . zip (iterate (+(-1)) t) $ iterate (+1) 0

lastWinningTime :: Int -> Int -> Int
lastWinningTime t d = (-) t . length . takeWhile not . fmap ((>d) . uncurry (*)) . take (t+1) . zip (iterate (+1) 0) $ iterate (+(-1)) t

part1 :: [Race] -> Int
part1 = foldr ((*) 
      . (\(Race t d) -> noOfWinningTimes t d)) 1

part2 :: [Race] -> Int
part2 = (+1)
      . uncurry (-)
      . bimap (uncurry lastWinningTime) (uncurry firstWinningTime)
      . (\a -> (a,a))
      . bimap (read
      . foldl' (++) []) (read
      . foldl' (++) []) . unzip 
      . fmap (\(Race t d) -> (show t, show d))
