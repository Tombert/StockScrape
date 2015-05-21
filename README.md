# StockScrape
An app to go and grab all the stock prices and spit back a comma-separated-list of all this info.


## How to run. 
```
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal run
 AMD,238 
 ...
```

## Note
I'm using Yahoo stocks to get the information, since their API is relatively straightforward, though I need to point out that there is a rate-limit imposed by Yahoo of 1000-requests per hour.  To help avoid this, I have batched-together 250 stocks per-request (the current maximum). This brings everything down to around 100 requests, so this shouldn't be an issue unless you run this script ten times an hour.

#### IF YOU DO NOT LIKE THIS, FORK AND ADD OAUTH SUPPORT
