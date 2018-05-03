tidychess
================
Josh Merrell

Purpose
-------

To transform chess data into formats compatible with statistics and machine learning in R.

Terminology
-----------

-   **game data**
-   **positional data**
-   **PGN format**
-   **FEN format**
-   **matrix format**
-   **tidy format**
-   **positional mobility**

Testing
-------

Load objects

``` r
# load functions and regex patterns (invisibly, to avoid clutter)
invisible(lapply(list.files("R", full.names = T), source))
```

    ## Loading required package: jsonlite

Sometimes scrape.live.R returns a data frame of lists:

``` r
# This issue was noticed for game ID 100002
DF <- scrape.live("https://www.chess.com/live/game/100002")

# print class of each column:
for(m in 1:ncol(DF)){print(paste(m, colnames(DF)[m], class(DF[, m])))}
```

    ## [1] "1 gameId list"
    ## [1] "2 initialSetup list"
    ## [1] "3 lastDate list"
    ## [1] "4 gameType list"
    ## [1] "5 plyCount list"
    ## [1] "6 playerColor list"
    ## [1] "7 whiteUsername list"
    ## [1] "8 blackUsername list"
    ## [1] "9 analysisPgn list"
    ## [1] "10 isCheckmate list"
    ## [1] "11 isStalemate list"
    ## [1] "12 analysisPublic list"
    ## [1] "13 Event character"
    ## [1] "14 Site character"
    ## [1] "15 Date character"
    ## [1] "16 White character"
    ## [1] "17 Black character"
    ## [1] "18 Result character"
    ## [1] "19 ECO character"
    ## [1] "20 WhiteElo character"
    ## [1] "21 BlackElo character"
    ## [1] "22 TimeControl character"
    ## [1] "23 EndTime character"
    ## [1] "24 Termination character"
    ## [1] "25 SetUp character"
    ## [1] "26 pgn character"
    ## [1] "27 FEN0 character"
