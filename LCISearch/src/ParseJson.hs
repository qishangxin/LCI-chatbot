{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ParseJson (readLCI, readLCIKeywords, writeLCIKeywords, writeOutput) where
import Data.Aeson (FromJSON, ToJSON (toEncoding, toJSON), eitherDecode, encodeFile, fromJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.List ( nub )
import qualified System.IO.Utf8 as Utf8


-- new Datatyps for the Json values
data LCIArticle = LCIArticle {saint :: String, text :: String} deriving (Show, Generic)
instance FromJSON LCIArticle

data LCIKeywords = LCIKeywords {keyword :: String, saints :: [Saint]} deriving (Show, Generic)
instance ToJSON LCIKeywords
instance FromJSON LCIKeywords

newtype Saint = Saint {saintName :: String} deriving (Show, Generic)
instance ToJSON Saint
instance FromJSON Saint

data Output = Output {name :: String, w_Count :: Int} deriving (Show, Generic)
instance ToJSON Output

pathToKeywords :: FilePath
pathToKeywords = "keywords.json"

pathToOutput :: FilePath 
pathToOutput = "ausgabe.json"

-- reads the LCIJson file
readLCI :: FilePath -> IO [(String, String)]
readLCI path = do 
    d <- eitherDecode <$> B.readFile path :: IO (Either String [LCIArticle])
    case d of
        Left error -> return [("Error LCIfile", [])]
        Right lci -> return $ zip (map getLCISaint lci) (map getLCIText lci)

-- reads the keywords from the keywordfile
readLCIKeywords :: IO [(String, [String])]
readLCIKeywords = do
    d <- eitherDecode <$> B.readFile pathToKeywords :: IO (Either String [LCIKeywords])
    case d of
        Left error -> return [("Error Keywordsfile", [])]
        Right keys -> return $ zip (map getKeyword keys) (map getKeywordSaints keys)

writeLCIKeywords :: [(String, [String])] -> IO ()
writeLCIKeywords a = encodeFile pathToKeywords $ toLCIKeywords a

writeOutput :: [(String, Int)] -> IO()
writeOutput a = encodeFile pathToOutput (map (uncurry Output) a)

-- getters
getLCISaint :: LCIArticle -> String
getLCISaint (LCIArticle saint _) = saint

getLCIText :: LCIArticle -> String
getLCIText (LCIArticle _ text) = text

getKeyword :: LCIKeywords -> String
getKeyword (LCIKeywords keys _) = keys

getKeywordSaints :: LCIKeywords -> [String]
getKeywordSaints (LCIKeywords _ saints) = map getSaint saints

getSaint :: Saint -> String 
getSaint (Saint s) = s

toLCIKeywords :: [(String, [String])] -> [LCIKeywords]
toLCIKeywords = map (\(name, saint) -> LCIKeywords name (map Saint (nub saint)))