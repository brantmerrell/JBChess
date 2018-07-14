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

**build data frame of chess positions**

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
pgn <- pgn[-length(pgn)]
pgn[seq(2,length(pgn),2)] <- paste0(seq(1,length(pgn)/2),
                                    "...",
                                    pgn[seq(2,length(pgn),2)])
for(n in 1:length(pgn)){
  positions <- rbind(positions,
                     new_position(pgn[n], positions))
  print(to.FEN(positions))
}; rm(n)
```

    ##                                                        1.e4 
    ## "rnbqkbnr/pppppppp/8/8/4P3/4P3/PPPP1PPP/RNBQKBNR b - e3  0" 
    ##                                                        1...e5 
    ## "rnbqkbnr/pppp1ppp/4p3/4p3/4P3/8/PPPP1PPP/RNBQKBNR w - e6  1" 
    ##                                                          2.f4 
    ## "rnbqkbnr/pppp1ppp/8/4p3/4PP2/5P2/PPPP2PP/RNBQKBNR b - f3  1" 
    ##                                                      2...d6 
    ## "rnbqkbnr/ppp2ppp/3p4/4p3/4PP2/8/PPPP2PP/RNBQKBNR w - -  2" 
    ##                                                         3.Bc4 
    ## "rnbqkbnr/ppp2ppp/3p4/4p3/2B1PP2/8/PPPP2PP/RNBQK1NR b - -  2" 
    ##                                                        3...c6 
    ## "rnbqkbnr/pp3ppp/2pp4/4p3/2B1PP2/8/PPPP2PP/RNBQK1NR w - -  3" 
    ##                                                          4.Nf3 
    ## "rnbqkbnr/pp3ppp/2pp4/4p3/2B1PP2/5N2/PPPP2PP/RNBQK2R b - -  3" 
    ##                                                         4...Bg4 
    ## "rn1qkbnr/pp3ppp/2pp4/4p3/2B1PPb1/5N2/PPPP2PP/RNBQK2R w - -  4" 
    ##                                                           5.fxe5 
    ## "rn1qkbnr/pp3ppp/2pp4/4P3/2B1P1b1/5N2/PPPP2PP/RNBQK2R b - - 0 4" 
    ##                                                        5...dxe5 
    ## "rn1qkbnr/pp3ppp/2p5/4p3/2B1P1b1/5N2/PPPP2PP/RNBQK2R w - - 0 5" 
    ##                                                       6.Bxf7+ 
    ## "rn1qkbnr/pp3Bpp/2p5/4p3/4P1b1/5N2/PPPP2PP/RNBQK2R b - - 0 5" 
    ##                                                      6...Kxf7 
    ## "rn1q1bnr/pp3kpp/2p5/4p3/4P1b1/5N2/PPPP2PP/RNBQK2R w - - 0 6" 
    ##                                                     7.Nxe5+ 
    ## "rn1q1bnr/pp3kpp/2p5/4N3/4P1b1/8/PPPP2PP/RNBQK2R b - - 0 6" 
    ##                                                    7...Ke8 
    ## "rn1qkbnr/pp4pp/2p5/4N3/4P1b1/8/PPPP2PP/RNBQK2R w - - 1 7" 
    ##                                                     8.Qxg4 
    ## "rn1qkbnr/pp4pp/2p5/4N3/4P1Q1/8/PPPP2PP/RNB1K2R b - - 0 7" 
    ##                                                      8...Nf6 
    ## "rn1qkb1r/pp4pp/2p2n2/4N3/4P1Q1/8/PPPP2PP/RNB1K2R w - - 1 8" 
    ##                                                      9.Qe6+ 
    ## "rn1qkb1r/pp4pp/2p1Qn2/4N3/4P3/8/PPPP2PP/RNB1K2R b - - 2 8" 
    ##                                                      9...Qe7 
    ## "rn2kb1r/pp2q1pp/2p1Qn2/4N3/4P3/8/PPPP2PP/RNB1K2R w - - 3 9" 
    ##                                                      10.Qc8+ 
    ## "rnQ1kb1r/pp2q1pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R b - - 4 9" 
    ##                                                    10...Qd8 
    ## "rnQqkb1r/pp4pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R w - - 5 10" 
    ##                                                    11.Qxd8+ 
    ## "rn1Qkb1r/pp4pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R b - - 0 10" 
    ##                                                   11...Kxd8 
    ## "rn1k1b1r/pp4pp/2p2n2/4N3/4P3/8/PPPP2PP/RNB1K2R w - - 0 11" 
    ##                                                    12.Nf7+ 
    ## "rn1k1b1r/pp3Npp/2p2n2/8/4P3/8/PPPP2PP/RNB1K2R b - - 1 11"

``` r
apply(positions[-1,], 1, mobility_board, 
      patterns = c(white="white", black="black", pawns="pawn", 
                   knights="knight", bishops="bishop", rooks="rook", 
                   queens="queen", kings="king"))
```

    ##         000_zero 1.e4 1...e5 2.f4 2...d6 3.Bc4 3...c6 4.Nf3 4...Bg4 5.fxe5
    ## total         40   51     61   62     64    68     69    70      72     71
    ## white         20   31     30   32     31    35     35    36      35     34
    ## black         20   20     31   30     33    33     34    34      37     37
    ## pawns         32   32     31   31     29    28     27    27      26     26
    ## knights        8    9     10   10     11    11     10    12      12     11
    ## bishops        0    5     10   10     11    15     15    15      17     17
    ## rooks          0    0      0    0      0     0      0     2       2      2
    ## queens         0    4      8    8      9     9     12     9      10     10
    ## kings          0    1      2    3      4     5      5     5       5      5
    ##         5...dxe5 6.Bxf7+ 6...Kxf7 7.Nxe5+ 7...Ke8 8.Qxg4 8...Nf6 9.Qe6+
    ## total         78      77       72      78      75     78      79     81
    ## white         33      33       25      30      30     42      42     44
    ## black         45      44       47      48      45     36      37     37
    ## pawns         23      22       22      21      21     21      21     22
    ## knights       12      12       12      15      15     14      17     18
    ## bishops       21      20       12      14      14      5       5      5
    ## rooks          2       2        2       2       2      2       3      3
    ## queens        15      15       16      18      17     29      26     26
    ## kings          5       6        8       8       6      7       7      7
    ##         9...Qe7 10.Qc8+ 10...Qd8 11.Qxd8+ 11...Kxd8 12.Nf7+
    ## total        73      71       76       69        58      56
    ## white        43      40       39       42        29      27
    ## black        30      31       37       27        29      29
    ## pawns        22      22       22       22        22      22
    ## knights      18      18       18       18        19      17
    ## bishops       0       0        5        5         5       5
    ## rooks         3       3        3        3         3       3
    ## queens       23      21       21       13         0       0
    ## kings         7       7        7        8         9       9
