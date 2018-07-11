tidychess
================
Josh Merrell

-   [Introduction](#introduction)
-   [Data Acquisition](#data-acquisition)
-   [Exploration of Functions](#exploration-of-functions)
-   [Exploration of Chessboards](#exploration-of-chessboards)

Introduction
============

**Terminology**
\* **game data** \* **positional data** \* **PGN format** \* **FEN format** \* **matrix format** \* **tidy format** \* **positional mobility** \* **pathPrior**: planning to change the name to *theoretical mobility*. Lists the squares a piece can move on an empty chessboard. \* **Post functions**: planning to change the name to *ingame* functions. They were named for *aposteriori*, and they list the squares to which a piece can move from a given position.

**Setup**

``` r
invisible(lapply(list.files("R", full.names = T), source))
```

    ## Loading required package: jsonlite

Data Acquisition
================

**Bash-managed scraping**

``` bash
#!/bin/bash
# obtain list of previously stored data to avoid duplication
aws s3 ls s3://jbchess/data --recursive > temp.txt

# iterate through a range of numbers
for n in $(seq 200 1000)
do

  # adjust both numbers to the proper order of magnitude
    first=$(($n*1000+1))
    last=$(($n*1000+1000))
    # record the beginning & ending range of numbers; include a timestamp
    echo start $first $last $(date)
    
    # pass the range of numbers to the scrape.chess.com function
    Rscript R/scrape.chess.com.R $first $last
    
    # move the acquired data to the bucket
    aws s3 mv data s3://jbchess/data --recursive
done
```

Exploration of Functions
========================

**pathPrior**

``` r
pathPrior(piece = "bishop", square = "e5")
```

    ##  [1] "a1" "b2" "b8" "c3" "c7" "d4" "d6" "f6" "f4" "g7" "g3" "h8" "h2"

``` r
pathPrior(piece = "knight", square = "e5")
```

    ## [1] "f7" "d7" "f3" "d3" "g6" "c6" "g4" "c4"

``` r
pathPrior(piece = "black pawn", square = "e5")
```

    ## [1] "e4" "d4" "f4"

``` r
pathPrior(piece = "white pawn", square = "e5")
```

    ## [1] "e6" "d6" "f6"

**position object**

``` r
position <- setup()
row.names(position)
```

    ## [1] "000_empty" "000_zero"

``` r
colnames(position)
```

    ##  [1] "a8" "a7" "a6" "a5" "a4" "a3" "a2" "a1" "b8" "b7" "b6" "b5" "b4" "b3"
    ## [15] "b2" "b1" "c8" "c7" "c6" "c5" "c4" "c3" "c2" "c1" "d8" "d7" "d6" "d5"
    ## [29] "d4" "d3" "d2" "d1" "e8" "e7" "e6" "e5" "e4" "e3" "e2" "e1" "f8" "f7"
    ## [43] "f6" "f5" "f4" "f3" "f2" "f1" "g8" "g7" "g6" "g5" "g4" "g3" "g2" "g1"
    ## [57] "h8" "h7" "h6" "h5" "h4" "h3" "h2" "h1"

**pathPost**

``` r
ls(pattern = ".Post")
```

    ## [1] "bishopPost."  "kingPost."    "knightPost."  "mobilityPost"
    ## [5] "pawnPost."    "piecePost."   "queenPost."   "rookPost."

``` r
pawnPost.(square = "a7", game_pgn = 2)
```

    ## [1] "a6" "a5"

``` r
bishopPost.(square = "c8", game_pgn = 2)
```

    ## character(0)

``` r
knightPost.(square = "b8", game_pgn = 2)
```

    ## [1] "c6" "a6"

``` r
rookPost.(square = "h8", game_pgn = 2)
```

    ## character(0)

``` r
queenPost.(square = "d1", game_pgn = 2)
```

    ## character(0)

``` r
kingPost.(square = "e8", game_pgn = 2)
```

    ## character(0)

``` r
mobilityPost(game_pgn = 2, piecePattern = chesspatterns$knight)
```

    ##          white black
    ## 000_zero     4     4

**compare post & prior**

``` r
pathPrior(piece = "knight", square = "b1")
pathPost.(square = "b1", game_pgn = "000_zero")
```

Because of the game\_pgn input, *pathPost.* knows that the piece on h2 is a white knight, and that it cannot move to **d2** because that space is occupied by another pawn.

``` r
for(n in 1:nrow(moves)){
  pgn <- paste("xmpl1",
               as.vector(moves[n,"pgn"]),
               sep="_")
  if(!pgn %in% row.names(position)){
    position<-rbind(position, newPosition(new_pgn = pgn))
  }
}
rownames(position)
```

Exploration of Chessboards
==========================
