{-# LANGUAGE DeriveDataTypeable #-}

module Main  where
import Data.List.Split 
import Data.String
import Network.HTTP
import Text.JSON.Generic
import Control.Concurrent.Async


(!&!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!&!) = flip valFromObj

data QueryObj = QueryObj
     {
        query ::ResultsObj 
     } deriving Show

data ResultsObj = ResultsObj
     {
       results :: QuoteObj 
     } deriving Show

data QuoteObj = QuoteObj 
     {
       quote :: [Stock]
     } deriving Show

data Stock = Stock 
     {
       sy :: String,
       daysHigh :: JSValue
     } deriving Show

getStocks :: QueryObj ->[Stock]
getStocks = quote . results . query 

instance JSON QueryObj where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        QueryObj     <$>
        obj !&! "query"

instance JSON ResultsObj where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        ResultsObj        <$>
        obj !&! "results"

instance JSON QuoteObj where 
    showJSON = undefined
    readJSON (JSObject obj) =
      QuoteObj <$>
      obj !&! "quote"

instance JSON Stock where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) = do
        s <- obj !&! "Symbol"
        dh <- obj !&! "DaysHigh"
     --   d <- obj !&! "Ask"
        return Stock{sy = s, daysHigh = dh}
        --Stock       <$>
        --obj !&! "symbol" <*>
        --obj !&! "daysHigh"
    --readJSON _ = mzero





quoteWrap x = "\"" ++ x ++ "\""

breakAndChunk = (splitEvery 250) . (map quoteWrap) . lines


commaList :: [String] -> String
commaList = tail . (foldl concatFields "")

parenWrap :: String -> String
parenWrap x = "(" ++ x ++ ")"

queryBuilder x = "use \"http://github.com/spullara/yql-tables/raw/d60732fd4fbe72e5d5bd2994ff27cf58ba4d3f84/yahoo/finance/yahoo.finance.quotes.xml\" as quotes; select * from quotes where symbol in " ++ x

urlBuilder :: String -> String
urlBuilder x = "http://query.yahooapis.com/v1/public/yql?format=json&q=" ++ x

makeUrl = urlBuilder . urlEncode . queryBuilder . parenWrap . commaList

concatFields :: String -> String -> String
concatFields x y = x ++ "," ++ y

-- Very simple helper function to do a basic get request 
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

-- Since there are only two type constructors for the Result typeclass, 
-- we don't need to do any kind of fancy Monad magic.  Instead, we can 
-- utilize pattern matching and just strip out the type constructors. 
removeResult :: Result a -> a
removeResult (Ok x) = x
removeResult (Error x) = error x 

getAllStocks =  (map getStocks) . (map removeResult) . (map (\x -> decode x :: Result QueryObj))


removeJSValue (JSString x) = fromJSString x
removeJSValue (JSNull) = "null"  

--prettyPrintList :: [Stock] -> String
prettyPrint = map (\x -> (concatFields (sy x) (removeJSValue (daysHigh x)) ++ "\n")) 

main :: IO ()
main = do 
         symbolsFile <- readFile "symbols.csv"
         let symbols = breakAndChunk symbolsFile
         let urls = map makeUrl symbols
         responses <- mapConcurrently get urls
         let myObjects = getAllStocks responses
         let yo = prettyPrint (concat myObjects)
         mapM_ putStrLn yo
         --mapM_ print myObjects 
