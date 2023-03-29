module Lib (searchMain) where

import ParseJson (readLCI, readLCIKeywords, writeLCIKeywords, writeOutput)
import Data.Text (isInfixOf, pack)
import Data.List (elemIndex, (\\), nub, elemIndices, intercalate)
import Data.Map (toList, fromListWith)

searchMain :: String -> [String] -> IO ()
searchMain pathToSearchJson searchAttributes = do
    lciData <- readLCI pathToSearchJson
    lciKeywords <- readLCIKeywords
    let search = searchLCI lciData (searchAttributes \\ map fst lciKeywords) (searchKeywords lciKeywords searchAttributes) in
        let clean = cleanSearch search in do
            writeLCIKeywords $ cleanSearch (clean ++ lciKeywords)
            writeOutput $ findBestMatch clean

searchLCI :: [(String, String)] -> [String] -> [(String, [String])] -> [(String, [String])]
searchLCI [] _ acc = acc
searchLCI _ [] acc = acc
searchLCI ((saint, text):xs) keys acc = let value = findInText text keys saint in searchLCI xs keys (value ++ acc)
    where
        findInText text [] saint = []
        findInText text (x:xs) saint | pack x `isInfixOf` pack text = findInText text xs saint ++ [(x, [saint])]
                                     | otherwise = findInText text xs saint ++ [(x, [])]

searchKeywords :: [(String, [String])] -> [String] -> [(String, [String])]
searchKeywords keywordsInLCI searchKeywords = helper keywordsInLCI searchKeywords []
    where
        helper _ [] acc = acc
        helper lciKeywords (x:xs) acc = case x `elemIndex` map fst lciKeywords of
            Just i -> helper lciKeywords xs (acc ++ [lciKeywords !! i])
            Nothing -> helper lciKeywords xs acc

cleanSearch :: [(String, [String])] -> [(String, [String])]
cleanSearch list = helper $ nub (map fst list)
    where
        helper [] = []
        helper (x:xs) = let indices = x `elemIndices` map fst list in
            helper xs ++ [(x, concatMap (\x -> snd (list !! x)) indices)]


findBestMatch :: [(String, [String])] -> [(String, Int)]
findBestMatch input = frequency $ concatMap snd input
    where
        frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])
