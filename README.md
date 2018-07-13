tidychess
================
Josh Merrell

-   [Introduction](#introduction)
-   [Exploration of Functions](#exploration-of-functions)
-   [Chess Data](#chess-data)

Introduction
============

**Terminology**

-   **game data**
-   **positional data**
-   **PGN format**
-   **FEN format**
-   **matrix format**
-   **tidy format**
-   **positional mobility**
-   **pathPrior**: planning to change the name to *theoretical mobility*. Lists the squares a piece can move on an empty chessboard.
-   **Post functions**: planning to change the name to *ingame* functions. They were named for *aposteriori*, and they list the squares to which a piece can move from a given position.

Exploration of Functions
========================

**Setup**

``` r
invisible(lapply(list.files("R", "\\.R$", full.names = T), source))
```

**The mobility\_piece function**

``` r
# if position_vec is not passed as an input, 
# it is assumed that no other pieces are on the board
mobility_piece(piece = "bishop", square = "e5")
```

    ##  [1] "a1" "b2" "b8" "c3" "c7" "d4" "d6" "f6" "f4" "g7" "g3" "h8" "h2"

``` r
# if board is empty, specified piece is not on specified square:
mobility_piece(piece = "white pawn", square = "a2", position_vec = empty())

# white pawns travel up and black pawns travel down
mobility_piece(piece = "black pawn", square = "e5")
```

    ## [1] "e4"

``` r
mobility_piece(piece = "white pawn", square = "e5")
```

    ## [1] "e6"

``` r
# pawns of unspecified color might go either direction
mobility_piece(piece = "pawn", square = "e5")
```

    ## [1] "e6?" "e4?"

``` r
# mobility_piece() piece type from position_vec & square:
mobility_piece(square = "a2", position_vec = setup(includeEmpty = F)[1,])
```

    ## [1] "a3" "a4"

``` r
# if piece input conflicts with square & position_vec inputs, it is overriden:
mobility_piece(square = "a2", 
               position_vec = setup(includeEmpty = F)[1,], 
               piece = "white rook")
```

    ## [1] "a3" "a4"

**compare post & prior**

``` r
mobility_board(position_vec = setup())
```

    ## total white black 
    ##    40    20    20

``` r
patterns <- c("white bishops" = "white bishop",
              "black knights"="black knight",
              "pawns"="pawn")
pieces <- unique(unlist(setup()[2,]));  pieces <- pieces[!is.na(pieces)]
mobility_board(position_vec = setup(), patterns = patterns)
```

    ##         total white bishops black knights         pawns 
    ##            40             0             4            32

``` r
mobility_board(position_vec = setup(), patterns = pieces)
```

    ##        total   black Rook   black pawn   white pawn   white Rook 
    ##           40            0           16           16            0 
    ## black Knight white Knight black Bishop white Bishop  black Queen 
    ##            4            4            0            0            0 
    ##  white Queen   black King   white King 
    ##            0            0            0

``` r
rm(patterns, pieces)
```

**new\_position**

``` r
positions <- setup()
positions <- rbind(positions, new_position(new_pgn = "1.e3",
                                           position_vec = positions))
positions[,c("e2","e3")]
rm(positions)
```

**to.FEN**

``` r
to.FEN(setup()[1,])
```

Chess Data
==========

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

**acquire sample of chess data**

``` bash
# use the shell to extract sample of chess games:
aws s3 cp "s3://jbchess/data/chess.com IDs 1-1000.csv" "data/chess.com IDs 1-1000.csv"
```

**build data frame of chess positions**

``` r
DATA <- read.csv("data/chess.com IDs 1-1000.csv", stringsAsFactors = F)
DATA <- DATA[grepl(".",DATA$pgn),]
DATA[1,]
```

    ##             Site       Date Round               White              Black
    ## 1 Madrid (Spain) 1560.??.??     ? Ruy Lopez De Segura Leonardo Da Curtie
    ##   Result ECO WhiteElo BlackElo Annotator Source Remark
    ## 1    1-0 C30        0        0        NA     NA     NA
    ##                                                                                                                                       pgn
    ## 1 1.e4 e5 2.f4 d6 3.Bc4 c6 4.Nf3 Bg4 5.fxe5 dxe5 6.Bxf7+ Kxf7 7.Nxe5+ Ke8 8.Qxg4 Nf6 9.Qe6+ Qe7 10.Qc8+ Qd8 11.Qxd8+ Kxd8 12.Nf7+ 1-0')">
    ##    ID Time Moves Termination
    ## 1 o_1

``` r
positions <- setup()
pgn <- strsplit(DATA[1,"pgn"]," ")[[1]]
pgn <- pgn[-length(pgn)]
pgn[seq(2,length(pgn),2)] <- paste0(seq(1,length(pgn)/2),
                                    "...",
                                    pgn[seq(2,length(pgn),2)])
for(elem_p in pgn){
  positions <- rbind(positions,
                     new_position(elem_p, positions))
}
apply(positions[-1,], 1, mobility_board, 
      patterns = c(white="white", black="black", pawns="pawn", 
                   knights="knight", bishops="bishop", rooks="rook", 
                   queens="queen", kings="king"))
```

    ##         000_zero 1.e4 1...e5 2.f4 2...d6 3.Bc4 3...c6 4.Nf3 4...Bg4 5.fxe5
    ## total         40   51     62   64     67    71     72    72      74     72
    ## white         20   31     31   33     33    37     37    37      36     34
    ## black         20   20     31   31     34    34     35    35      38     38
    ## pawns         32   32     32   33     32    31     30    29      28     27
    ## knights        8    9     10   10     11    11     10    12      12     11
    ## bishops        0    5     10   10     11    15     15    15      17     17
    ## rooks          0    0      0    0      0     0      0     2       2      2
    ## queens         0    4      8    8      9     9     12     9      10     10
    ## kings          0    1      2    3      4     5      5     5       5      5
    ##         5...dxe5 6.Bxf7+ 6...Kxf7 7.Nxe5+ 7...Ke8 8.Qxg4 8...Nf6 9.Qe6+
    ## total         80      79       74      80      77     80      81     82
    ## white         34      34       26      31      31     43      43     45
    ## black         46      45       48      49      46     37      38     37
    ## pawns         25      24       24      23      23     23      23     23
    ## knights       12      12       12      15      15     14      17     18
    ## bishops       21      20       12      14      14      5       5      5
    ## rooks          2       2        2       2       2      2       3      3
    ## queens        15      15       16      18      17     29      26     26
    ## kings          5       6        8       8       6      7       7      7
    ##         9...Qe7 10.Qc8+ 10...Qd8 11.Qxd8+ 11...Kxd8 12.Nf7+
    ## total        74      72       77       70        59      57
    ## white        44      41       40       43        30      28
    ## black        30      31       37       27        29      29
    ## pawns        23      23       23       23        23      23
    ## knights      18      18       18       18        19      17
    ## bishops       0       0        5        5         5       5
    ## rooks         3       3        3        3         3       3
    ## queens       23      21       21       13         0       0
    ## kings         7       7        7        8         9       9

ISSUE: phantom black pawn not being deleted
