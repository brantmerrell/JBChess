tidychess
================
Josh Merrell

-   [Introduction](#introduction)
-   [Exploration of Functions](#exploration-of-functions)
-   [Chess Data](#chess-data)
-   [Issues](#issues)

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
# returns empty string in response to empty chessboard
to.FEN(empty())

# returns standard string for standard setup
to.FEN(setup())
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

**example of positional data**

``` r
DATA <- read.csv("data/chess.com IDs 1-1000.csv", stringsAsFactors = F)
DFSummary(DATA)
```

    ##       variable     class unique is.na has.content empty.strings
    ## 1         Site character     44     0         984          1402
    ## 2         Date character     47     0        1000          1386
    ## 3        Round character     33     0        1000          1386
    ## 4        White character   1026     0        2386             0
    ## 5        Black character   1041     0        2386             0
    ## 6       Result character      6     0        2267           119
    ## 7          ECO character    100     0        1000          1386
    ## 8     WhiteElo   integer    697     0        2386             0
    ## 9     BlackElo   integer    718     0        2386             0
    ## 10   Annotator   logical      1  2386           0             0
    ## 11      Source   logical      1  2386           0             0
    ## 12      Remark   logical      1  2386           0             0
    ## 13         pgn character   1001     0        1000          1386
    ## 14          ID character   2384     0        2386             0
    ## 15        Time character    127     1        1385          1000
    ## 16       Moves character     80     0        1386          1000
    ## 17 Termination character      6     0        1386          1000

``` r
positions <- setup()
pgn <- strsplit(DATA[1,"pgn"]," ")[[1]]
pgn <- pgn[!grepl(">", pgn)]
pgn[seq(2,length(pgn),2)] <- paste0(seq(1,length(pgn)/2),
                                    "...",
                                    pgn[seq(2,length(pgn),2)])
for(n in 1:length(pgn)){
  positions <- rbind(positions,
                     new_position(pgn[n], positions))
}; rm(n)
for(n in 2:nrow(positions)){
  positions[n, "FEN"] <- to.FEN(positions[1:n,1:64])
}
print(cbind(row.names(positions),positions$FEN))
```

    ##       [,1]       
    ##  [1,] "000_empty"
    ##  [2,] "000_zero" 
    ##  [3,] "1.e4"     
    ##  [4,] "1...e5"   
    ##  [5,] "2.f4"     
    ##  [6,] "2...d6"   
    ##  [7,] "3.Bc4"    
    ##  [8,] "3...c6"   
    ##  [9,] "4.Nf3"    
    ## [10,] "4...Bg4"  
    ## [11,] "5.fxe5"   
    ## [12,] "5...dxe5" 
    ## [13,] "6.Bxf7+"  
    ## [14,] "6...Kxf7" 
    ## [15,] "7.Nxe5+"  
    ## [16,] "7...Ke8"  
    ## [17,] "8.Qxg4"   
    ## [18,] "8...Nf6"  
    ## [19,] "9.Qe6+"   
    ## [20,] "9...Qe7"  
    ## [21,] "10.Qc8+"  
    ## [22,] "10...Qd8" 
    ## [23,] "11.Qxd8+" 
    ## [24,] "11...Kxd8"
    ## [25,] "12.Nf7+"  
    ##       [,2]                                                             
    ##  [1,] NA                                                               
    ##  [2,] "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -  0"        
    ##  [3,] "rnbqkbnr/pppppppp/8/8/4P3/4P3/PPPP1PPP/RNBQKBNR b KQk e3  0"    
    ##  [4,] "rnbqkbnr/pppp1ppp/4p3/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQk e6  1"  
    ##  [5,] "rnbqkbnr/pppp1ppp/8/4p3/4PP2/5P2/PPPP2PP/RNBQKBNR b KQk f3  1"  
    ##  [6,] "rnbqkbnr/ppp2ppp/3p4/4p3/4PP2/8/PPPP2PP/RNBQKBNR w KQk -  2"    
    ##  [7,] "rnbqkbnr/ppp2ppp/3p4/4p3/2B1PP2/8/PPPP2PP/RNBQK1NR b KQk -  2"  
    ##  [8,] "rnbqkbnr/pp3ppp/2pp4/4p3/2B1PP2/8/PPPP2PP/RNBQK1NR w KQk -  3"  
    ##  [9,] "rnbqkbnr/pp3ppp/2pp4/4p3/2B1PP2/5N2/PPPP2PP/RNBQK2R b KQk -  3" 
    ## [10,] "rn1qkbnr/pp3ppp/2pp4/4p3/2B1PPb1/5N2/PPPP2PP/RNBQK2R w KQk -  4"
    ## [11,] "rn1qkbnr/pp3ppp/2pp4/4P3/2B1P1b1/5N2/PPPP2PP/RNBQK2R b KQk - 4" 
    ## [12,] "rn1qkbnr/pp3ppp/2p5/4p3/2B1P1b1/5N2/PPPP2PP/RNBQK2R w KQk - 5"  
    ## [13,] "rn1qkbnr/pp3Bpp/2p5/4p3/4P1b1/5N2/PPPP2PP/RNBQK2R b KQk - 5"    
    ## [14,] "rn1q1bnr/pp3kpp/2p5/4p3/4P1b1/5N2/PPPP2PP/RNBQK2R w KQ - 6"     
    ## [15,] "rn1q1bnr/pp3kpp/2p5/4N3/4P1b1/8/PPPP2PP/RNBQK2R b KQ - 6"       
    ## [16,] "rn1qkbnr/pp4pp/2p5/4N3/4P1b1/8/PPPP2PP/RNBQK2R w KQ - 7"        
    ## [17,] "rn1qkbnr/pp4pp/2p5/4N3/4P1Q1/8/PPPP2PP/RNB1K2R b KQ - 7"        
    ## [18,] "rn1qkb1r/pp4pp/2p2n2/4N3/4P1Q1/8/PPPP2PP/RNB1K2R w KQ - 8"      
    ## [19,] "rn1qkb1r/pp4pp/2p1Qn2/4N3/4P3/8/PPPP2PP/RNB1K2R b KQ - 8"       
    ## [20,] "rn2kb1r/pp2q1pp/2p1Qn2/4N3/4P3/8/PPPP2PP/RNB1K2R w KQ - 9"      
    ## [21,] "rnQ1kb1r/pp2q1pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R b KQ - 9"      
    ## [22,] "rnQqkb1r/pp4pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R w KQ - 10"       
    ## [23,] "rn1Qkb1r/pp4pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R b KQ - 10"       
    ## [24,] "rn1k1b1r/pp4pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R w KQ - 11"       
    ## [25,] "rn1k1b1r/pp3Npp/2p2n2/8/4P3/8/PPPP2PP/RNB1K2R b KQ - 11"

``` r
apply(positions[-1,], 1, mobility_board, 
      patterns = c(white="white", black="black", pawns="pawn", 
                   knights="knight", bishops="bishop", rooks="rook", 
                   queens="queen", kings="king"))
```

    ##         000_zero 1.e4 1...e5 2.f4 2...d6 3.Bc4 3...c6 4.Nf3 4...Bg4 5.fxe5
    ## total         40   42     52   62     58    67     66    68      70     71
    ## white         20   22     30   32     31    34     35    34      35     34
    ## black         20   20     22   30     27    33     31    34      35     37
    ## pawns         32   32     31   31     29    28     27    27      26     26
    ## knights        8    9     10   10     11    11     10    12      12     11
    ## bishops        0    0      5   10      6    14     15    15      16     17
    ## rooks          0    0      0    0      0     0      0     0       2      2
    ## queens         0    0      4    8      8     9      9     9       9     10
    ## kings          0    1      2    3      4     5      5     5       5      5
    ##         5...dxe5 6.Bxf7+ 6...Kxf7 7.Nxe5+ 7...Ke8 8.Qxg4 8...Nf6 9.Qe6+
    ## total         69      74       71      73      75     77      78     78
    ## white         33      30       25      28      30     41      42     41
    ## black         36      44       46      45      45     36      36     37
    ## pawns         23      21       22      21      21     21      21     21
    ## knights       12      12       12      15      15     14      17     18
    ## bishops       17      18       12      11      14      5       5      5
    ## rooks          2       2        2       2       2      2       2      3
    ## queens        10      15       15      16      17     28      26     24
    ## kings          5       6        8       8       6      7       7      7
    ##         9...Qe7 10.Qc8+ 10...Qd8 11.Qxd8+ 11...Kxd8 12.Nf7+
    ## total        72      65       70       67        58      56
    ## white        43      36       39       40        29      27
    ## black        29      29       31       27        29      29
    ## pawns        22      22       22       22        22      22
    ## knights      18      18       18       18        19      17
    ## bishops       0       0        0        5         5       5
    ## rooks         3       3        3        3         3       3
    ## queens       22      15       20       11         0       0
    ## kings         7       7        7        8         9       9

``` r
rm(positions, pgn, DATA)
```

\*\*iterating through positional data

``` r
DATA <- read.csv("data/chess.com IDs 1-1000.csv", stringsAsFactors = F)
SEQ <- grep(".", DATA$pgn)
SEQ <- SEQ[seq(1, length(SEQ), length.out = 10)]
for(n in SEQ){
  positions <- setup()
  pgn <- strsplit(DATA[n,"pgn"], " ")[[1]]
  pgn <- pgn[!grepl(">", pgn)]
  pgn[seq(2,length(pgn),2)] <- paste0(seq(1,length(pgn)/2),
                                    "...",
                                    pgn[seq(2,length(pgn),2)])
  for(i in 1:length(pgn)){
    positions <- rbind(positions,
                       new_position(pgn[i], positions))
  }; rm(i)
  DATA[n,"FEN"] <- to.FEN(position_df=positions[,1:64])
  print(n)
}; rm(n, pgn)
DATA[SEQ,c("ID", "FEN")]
table(nchar(DATA$FEN))
```

Issues
======

``` r
rm(list=ls())
invisible(lapply(list.files("R", "\\.R$", full.names = T), source))

DATA <- read.csv("data/chess.com IDs 1-1000.csv", stringsAsFactors = F)
positions <- setup()

pgn <- strsplit(DATA[1,"pgn"]," ")[[1]]
pgn <- pgn[!grepl(">", pgn)]
pgn[seq(2,length(pgn),2)] <- paste0(seq(1,length(pgn)/2),
                                    "...",
                                    pgn[seq(2,length(pgn),2)])
for(n in 1:length(pgn)){
  temp <- new_position(pgn[n], positions)
  if(class(temp[,1])=="factor"){stop("strings are factors")}
  if(class(temp)!="data.frame"){stop("not a data frame")}
  positions <- rbind(positions,
                     temp)
  n <- n + 1
}
to.FEN(positions)
# this board is correct:
board(positions[nrow(positions),])
file.edit("R/to.FEN.R")
```
