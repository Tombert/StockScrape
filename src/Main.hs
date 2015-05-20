{-# LANGUAGE DeriveDataTypeable #-}

module Main  where
import Data.List.Split 
import Data.String
import Network.HTTP
import Text.JSON.Generic


data Stock = Stock 
     {
       symbol :: String,
       daysHigh :: String,
       daysLow  :: String
     }


quoteWrap x = "\"" ++ x ++ "\""

breakAndChunk = (splitEvery 250) . (map quoteWrap) . lines


commaList :: [String] -> String
commaList = tail . (foldl concatFields "")

parenWrap :: String -> String
parenWrap x = "(" ++ x ++ ")"

queryBuilder x = "use \"http://github.com/spullara/yql-tables/raw/d60732fd4fbe72e5d5bd2994ff27cf58ba4d3f84/yahoo/finance/yahoo.finance.quotes.xml\" as quotes; select * from quotes where symbol in " ++ x

urlBuilder x = "http://query.yahooapis.com/v1/public/yql?format=json&q=" ++ x

makeUrl = urlBuilder . urlEncode . queryBuilder . parenWrap . commaList

concatFields :: String -> String -> String
concatFields x y = x ++ "," ++ y

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

main :: IO ()
main = do 
         symbolsFile <- readFile "symbols.csv"
         let symbols = breakAndChunk symbolsFile
         let urls = map makeUrl symbols
         let responses = map get urls
         --lah <- map (>>=) responses
         mapM_ (>>=print) responses 
         --respgonse <- het ("http://query.yahooapis.com/v1/public/yql?format=json&amp;q=" ++ (queries !! 0))
         --
         --putStrLn (queries !! 0) 
