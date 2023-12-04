{-# LANGUAGE TupleSections #-}

import Data.List (foldl', sort)
import Data.IntSet qualified as IntSet
import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.Vector qualified as V
import Data.Bifunctor (bimap, first)
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap qualified as IntMap
import Data.Char
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let part = read $ head args
    case part of
        1 -> BS.interact (BS.pack . show . part1 . fmap parseCard . BS.lines)
        2 -> BS.interact (BS.pack . show . part2 . fmap parseCard .  BS.lines)
        _ -> undefined

data Card a = Card a [a] [a]
    deriving (Show, Eq)

parseCard :: ByteString -> Card Int
parseCard s = 
    let [leftToColon, rightToColon] = BS.split ':' s
        cardId = read . BS.unpack $ BS.drop 5 leftToColon
        [winningVals, vals] = fmap (read . BS.unpack) . BS.words <$> BS.split '|' rightToColon
    in Card cardId winningVals vals

numOfMatches :: Card Int -> Int
numOfMatches (Card _ ws vs) = IntSet.size $ IntSet.fromList ws `IntSet.intersection` IntSet.fromList vs

cardScore :: Card Int -> Int
cardScore = (\x -> if x > 0 then 2 ^ (x-1) else 0) . numOfMatches

wonCards :: Card Int -> [Int]
wonCards card@(Card n _ _)=  (\l -> [n+1..n+l]) $ numOfMatches card

part1 :: [Card Int] -> Int
part1 = sum . fmap (fromIntegral . cardScore)

part2 :: [Card Int] -> Int
part2 cards = 
    let n = length cards
        count :: [Int] = IntMap.elems $ 
                foldl'
                (\freq card@(Card i _ _) -> 
                    let count = freq IntMap.! i
                    -- Update the count for the cards that we won by the count of the current card
                    in foldl' (\f wonCard -> IntMap.insertWith (+) wonCard count f) freq $ wonCards card
                ) 
                (IntMap.fromList $ map (,1) [1..n])
                cards
    in sum count
