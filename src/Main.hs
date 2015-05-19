module Main  where
import Data.List.Split 

quoteWrap x = "\"" ++ x ++ "\""

breakAndChunk = (splitEvery 100) . (map quoteWrap) . lines


commaList :: [String] -> String
commaList = tail . (foldl concatFields "")

parenWrap :: String -> String
parenWrap x = "(" ++ x ++ ")"

queryBuilder x = "select * from yahoo.finance.quote where symbol in " ++ x

makeQuery = queryBuilder . parenWrap . commaList

concatFields :: String -> String -> String
concatFields x y = x ++ "," ++ y

main :: IO ()
main = do 
         symbolsFile <- readFile "symbols.csv"
         let symbols = breakAndChunk symbolsFile
         let queries = map makeQuery symbols

         putStrLn (queries!!0) 
