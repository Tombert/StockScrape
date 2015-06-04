{-# LANGUAGE DeriveDataTypeable #-}

module Main  where
import Data.List.Split 
import Data.String
import Network.HTTP
import Text.JSON.Generic
import Control.Concurrent.Async



-- This is a quick helper to allow me to add the object first over the value. 
(!&!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!&!) = flip valFromObj

-- I don't really like how this item looks, but it's just there to grab query
-- item out of the json object
data QueryObj = QueryObj
     {
        query ::ResultsObj 
     } deriving Show

-- This grabs the result out of the JSON object. 
data ResultsObj = ResultsObj
     {
       results :: QuoteObj 
     } deriving Show

-- This grabs the quote out of the JSON object...Yahoo really does nest this data 
-- way too much. 
data QuoteObj = QuoteObj 
     {
       quote :: [Stock]
     } deriving Show

-- Here's the meat of it all; this is a handle on the symbol and the daysHigh value 
-- from the stock object.  
data Stock = Stock 
     {
       sy :: String,
       daysHigh :: JSValue
     } deriving Show


-- God I write a lot of helper functions.  Anyway, this
-- is just a quick composition to extract out the stocks from
-- all these intermediate objects. 
getStocks :: QueryObj ->[Stock]
getStocks = quote . results . query 


-- This is the first handle on the JSON, and it passes the processing
-- off to results. 
instance JSON QueryObj where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        QueryObj     <$>
        obj !&! "query"

-- This  hands processing off to the Quote
instance JSON ResultsObj where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        ResultsObj        <$>
        obj !&! "results"


-- This hands processing off to the Stock
instance JSON QuoteObj where 
    showJSON = undefined
    readJSON (JSObject obj) =
      QuoteObj <$>
      obj !&! "quote"


-- And this finally grabs values and places them into an object we can 
-- use.  Once we have an object, it's relatively straightforward to make
-- into a comma-separated list. 
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




-- The name is self explanatory here. 
quoteWrap x = "\"" ++ x ++ "\""

-- This is a quick composition to break up a file by newline, wrap it
-- in quotes, then break it up into 1000 element chunks. 
breakAndChunk = (chunksOf 1000) . (map quoteWrap) . lines


commaList :: [String] -> String
commaList = tail . (foldl concatFields "")

parenWrap :: String -> String
parenWrap x = "(" ++ x ++ ")"

queryBuilder :: String -> String
queryBuilder x = "use \"http://github.com/spullara/yql-tables/raw/d60732fd4fbe72e5d5bd2994ff27cf58ba4d3f84/yahoo/finance/yahoo.finance.quotes.xml\" as quotes; select * from quotes where symbol in " ++ x



urlBuilder :: String -> String
urlBuilder x = "http://query.yahooapis.com/v1/public/yql?format=json&q=" ++ x



-- Gotta love function composition.  This is a quick helper function that 
-- converts a list of strings to comma-separated, then wraps them in parens,
-- builds a yql query, then makes it safe for URLs, then builds a URL out of it. 
makeUrl = urlBuilder . urlEncode . queryBuilder . parenWrap . commaList



-- This is a very simple function that takes in two strings and puts a comma 
-- between them.  Not particularly useful by itself; it's handy for folds. 
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

-- This is a hack; There's a very-finit amount of type constructors for this JSValue thing, 
-- and all I really care about are strings and nulls.  As such, I can just pattern-match on 
-- type constructors and grab out the values. 
removeJSValue :: JSValue -> String
removeJSValue (JSString x) = fromJSString x
removeJSValue _ = "null"  

jsonToAllStocks = getStocks . removeResult . (\x -> decode x :: Result QueryObj)
-- This is just a quick helper function to convert all the JSON into stocks
getAllStocks =  map jsonToAllStocks

prettyPrint = map (\x -> concatFields (sy x) (removeJSValue (daysHigh x))) 

main :: IO ()
main = do
         
         -- This probably should change; right now we have a file that contains all
         -- the stock symbols, which is just a big CSV with no commas, only newlines. 
         symbolsFile <- readFile "symbols.csv"
         
         -- Now that we have a handle on the symbols, let's get them into proper chunks
         -- to happily handle yahoo's rate-limit. 
         let symbols = breakAndChunk symbolsFile
         
         -- Now that we have all the symbols that are properly chunked, 
         -- we can make a big list of URLs to send to yahoo
         let urls = map makeUrl symbols
         
         -- mapConcurrently does what it says, but a note should be that it's a monadic-map, 
         -- hence the <-. 
         responses <- mapConcurrently get urls
         let myObjects = getAllStocks responses
         let yo = prettyPrint (concat myObjects)
         mapM_ putStrLn yo
