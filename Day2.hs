{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import System.Environment
import Data.List (sort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    let part = read $ args !! 0
    case part of
        1 -> BS.interact (BS.pack . show . part1 . fmap parseGame . BS.lines)
        2 -> BS.interact (BS.pack . show . part2 . fmap parseGame . BS.lines)
        _ -> undefined

data Game = Game Int [Turn]
    deriving (Show, Eq)

newtype Turn = Turn { draws :: [Draw]}
    deriving (Show, Eq)

data Draw = Draw Color Int
    deriving (Show, Eq)

data Color = Red | Green | Blue
    deriving (Show, Eq, Ord)

parseGame :: BS.ByteString -> Game
parseGame line = 
    let [gameB, turnsB] = BS.split ':' line
        gameId = read . BS.unpack $ BS.filter isDigit gameB
    in Game gameId . fmap parseTurn $ BS.split ';' turnsB

parseTurn :: BS.ByteString -> Turn
parseTurn = Turn . fmap parseDraw . BS.split ','

parseDraw :: BS.ByteString -> Draw
parseDraw draw =
    case b of 
        "red" -> Draw Red n
        "green" -> Draw Green n
        "blue" -> Draw Blue n
    where
        (a:b:_) = BS.split ' ' $ BS.tail draw
        n = read . BS.unpack $ a

part1 :: [Game] -> Int
part1 [] = 0
part1 ((Game gameId turns):xs) =
    ((fromEnum $ all checkTurnValidity turns) * gameId) + part1 xs

checkTurnValidity :: Turn -> Bool
checkTurnValidity =
    let checkDrawValidity draw = 
            case draw of
                Draw Red n -> n <= 12
                Draw Green n -> n <= 13
                Draw Blue n -> n <= 14
    in all checkDrawValidity . draws

part2 :: [Game] -> Int
part2 [] = 0
part2 ((Game _ turns):xs) =
    let req = foldr (\turn m -> M.unionWith max m $ minReq turn) M.empty turns
    in (foldr (*) 1 $ M.elems req) + part2 xs

minReq :: Turn -> M.Map Color Int
minReq = M.fromList . fmap (\(Draw c n) -> (c, n)) . draws
