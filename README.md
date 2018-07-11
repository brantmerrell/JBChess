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
lapply(list.files("R", full.names = T), source)
```

    ## Loading required package: jsonlite

    ## [[1]]
    ## [[1]]$value
    ## function (square1, square2, game_pgn) 
    ## {
    ##     Col1 <- which(letters == strsplit(square1, "")[[1]][1])
    ##     Row1 <- as.numeric(strsplit(square1, "")[[1]][2])
    ##     Col2 <- which(letters == strsplit(square2, "")[[1]][1])
    ##     Row2 <- as.numeric(strsplit(square2, "")[[1]][2])
    ##     if (length(Col1:Col2) != length(Row1:Row2) & !(0 %in% c(Col1 - 
    ##         Col2, Row1 - Row2))) {
    ##         stop("these two squares do not share rank, file, or diagnal")
    ##     }
    ##     btwns <- paste(letters[Col1:Col2], Row1:Row2, sep = "")
    ##     return(data.frame(squares = btwns, occupants = as.character(position[game_pgn, 
    ##         btwns])))
    ## }
    ## 
    ## [[1]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[2]]
    ## [[2]]$value
    ## function (square, game_pgn) 
    ## {
    ##     Col <- which(letters == strsplit(square, "")[[1]][1])
    ##     Row <- as.numeric(strsplit(square, "")[[1]][2])
    ##     Lim <- min(length(Col:8), length(Row:8))
    ##     Path <- paste(letters[Col:8][1:Lim], (Row:8)[1:Lim], sep = "")
    ##     Lim <- min(length(Col:1), length(Row:1))
    ##     Path <- c(Path, paste(letters[Col:1][1:Lim], (Row:1)[1:Lim], 
    ##         sep = ""))
    ##     Lim <- min(length(Col:8), length(Row:1))
    ##     Path <- c(Path, paste(letters[Col:8][1:Lim], (Row:1)[1:Lim], 
    ##         sep = ""))
    ##     Lim <- min(length(Col:1), length(Row:8))
    ##     Path <- c(Path, paste(letters[Col:1][1:Lim], (Row:8)[1:Lim], 
    ##         sep = ""))
    ##     Path <- Path[Path != square]
    ##     Path <- sort(unique(Path))
    ##     Test <- function(toSquare, fromSquare = square) {
    ##         fromColor <- pieceColor(fromSquare, game_pgn)
    ##         if (!is.na(fromColor)) {
    ##             if (fromColor == chesspatterns$white) {
    ##                 colorPattern <- chesspatterns$black
    ##             }
    ##             if (fromColor == chesspatterns$black) {
    ##                 colorPattern <- chesspatterns$white
    ##             }
    ##         }
    ##         else {
    ##             colorPattern <- paste(chesspatterns$white, chesspatterns$black, 
    ##                 sep = "|")
    ##         }
    ##         toColor <- pieceColor(toSquare, game_pgn)
    ##         if (grepl("phantom", position[game_pgn, toSquare])) {
    ##             toColor <- NA
    ##         }
    ##         DF <- betweens(toSquare, fromSquare, game_pgn)
    ##         T1 <- (sum(is.na(DF$occupants[-c(1, nrow(DF))]) | grepl("phantom", 
    ##             DF$occupants[-c(1, nrow(DF))])) == nrow(DF) - 2)
    ##         T2a <- grepl(colorPattern, DF[DF$squares == toSquare, 
    ##             "occupants"])
    ##         T2b <- is.na(DF[DF$squares == toSquare, "occupants"]) | 
    ##             grepl("phantom", DF[DF$squares == toSquare, "occupants"])
    ##         T1 & (T2a | T2b)
    ##     }
    ##     Path[unlist(lapply(Path, Test))]
    ## }
    ## 
    ## [[2]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[3]]
    ## [[3]]$value
    ## function (game_pgn = nrow(position), command = "View,return", 
    ##     wordSep = " ") 
    ## {
    ##     DF <- data.frame(matrix(position[game_pgn, ], nrow = 8, ncol = 8), 
    ##         row.names = 8:1)
    ##     colnames(DF) <- letters[1:8]
    ##     DF <- apply(DF, c(1, 2), gsub, pattern = " ", replacement = wordSep)
    ##     dimnames(DF) <- list(8:1, letters[1:8])
    ##     if (grepl("View", command)) {
    ##         View(DF)
    ##     }
    ##     if (grepl("return", command)) {
    ##         return(as.matrix(DF))
    ##     }
    ## }
    ## 
    ## [[3]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[4]]
    ## [[4]]$value
    ## function (pgn) 
    ## {
    ##     if (grepl("(?<!\\.)\\.{3}(?!\\.)", pgn, perl = T)) {
    ##         return("black")
    ##     }
    ##     if (grepl("(?<!\\.)\\.{1}(?!\\.)", pgn, perl = T)) {
    ##         return("white")
    ##     }
    ##     return(NA)
    ## }
    ## 
    ## [[4]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[5]]
    ## [[5]]$value
    ## function (ID) 
    ## {
    ##     sample1 <- sort(as.numeric(subset(chesspgn$move, chesspgn$ID == 
    ##         ID & chesspgn$color == "white")))
    ##     test1 <- ifelse((length(sample1) != 0), identical(paste(sample1), 
    ##         paste(1:max(sample1))), FALSE)
    ##     sample2 <- sort(as.numeric(subset(chesspgn$move, chesspgn$ID == 
    ##         ID & chesspgn$color == "black")))
    ##     test2 <- ifelse((length(sample2) != 0), identical(paste(sample2), 
    ##         paste(1:max(sample2))), FALSE)
    ##     ifelse((test1 == TRUE & test2 == TRUE), return("complete"), 
    ##         return("incomplete"))
    ## }
    ## 
    ## [[5]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[6]]
    ## [[6]]$value
    ## function (X) 
    ## {
    ##     if (grepl("O-O", X)) {
    ##         kingColumn <- "g"
    ##         rookColumn <- "f"
    ##         rookVacant <- "h"
    ##     }
    ##     if (grepl("O-O-O", X)) {
    ##         kingColumn <- "c"
    ##         rookColumn <- "d"
    ##         rookVacant <- "a"
    ##     }
    ##     if (grepl(chesspatterns$white, X)) {
    ##         Row <- 1
    ##     }
    ##     if (grepl(chesspatterns$black, X)) {
    ##         Row <- 8
    ##     }
    ##     list(king = paste(kingColumn, Row, sep = ""), rook = paste(rookColumn, 
    ##         Row, sep = ""), kingVacant = paste("e", Row, sep = ""), 
    ##         rookVacant = paste(rookVacant, Row, sep = ""))
    ## }
    ## 
    ## [[6]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[7]]
    ## [[7]]$value
    ## function (n = 1, incolor = c("white", "black")) 
    ## {
    ##     chesspgn <- "C:/Users/Josh/Documents/chess/chesspgn.csv"
    ##     chesspgn <- read.csv(chesspgn, colClasses = "character")
    ##     while (sum(chesspgn$move %in% n) < 500 & max(n) < max(as.numeric(chesspgn$move))) {
    ##         n <- c(n, (max(n) + 1))
    ##     }
    ##     workframe <- subset(chesspgn, chesspgn$color %in% incolor & 
    ##         chesspgn$move %in% n)
    ##     if (nchar(min(n)) == 1) {
    ##         filepath <- paste("C:/Users/Josh/Documents/Chess/move_0")
    ##     }
    ##     if (nchar(min(n)) == 2) {
    ##         filepath <- paste("C:/Users/Josh/Documents/Chess/move_")
    ##     }
    ##     if (2 < nchar(min(n))) {
    ##         filepath <- paste("C:/Users/Josh/Documents/Chess/move_0")
    ##         message("I might have to adjust the filenames for this length of game")
    ##     }
    ##     filepath <- paste(filepath, min(n), ".csv", sep = "")
    ##     if (500 <= nrow(workframe)) {
    ##         write.csv(workframe, filepath, row.names = FALSE)
    ##         print(filepath)
    ##         sampleframe <- rbind(workframe[1, ], workframe[as.integer(nrow(workframe) * 
    ##             0.2), ], workframe[as.integer(nrow(workframe) * 0.4), 
    ##             ], workframe[as.integer(nrow(workframe) * 0.6), ], 
    ##             workframe[as.integer(nrow(workframe) * 0.8), ], workframe[as.integer(nrow(workframe)), 
    ##                 ])
    ##     }
    ##     else {
    ##         sampleframe <- tail(workframe, 3)
    ##     }
    ##     sumframe <- data.frame(King = sum(workframe$piece == "King"), 
    ##         Queen = sum(workframe$piece == "Queen"), Rook = sum(workframe$piece == 
    ##             "Rook"), Knight = sum(workframe$piece == "Knight"), 
    ##         Bishop = sum(workframe$piece == "Bishop"), pawn = sum(workframe$piece == 
    ##             "pawn"), total = nrow(workframe))
    ##     print(sampleframe)
    ##     print(sumframe)
    ## }
    ## 
    ## [[7]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[8]]
    ## [[8]]$value
    ## function (gameName = "000") 
    ## {
    ##     board <- expand.grid(columns = letters[1:8], rows = 1:8)
    ##     board <- board[order((board$columns), rev(board$rows)), ]
    ##     board <- paste(board[, 1], board[, 2], sep = "")
    ##     squares <- board
    ##     position <- data.frame(matrix(NA, nrow = 1, ncol = 64))
    ##     row.names(position) <- paste(gameName, "empty", sep = "_")
    ##     colnames(position) <- squares
    ##     position
    ## }
    ## 
    ## [[8]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[9]]
    ## [[9]]$value
    ## function (square, game_pgn) 
    ## {
    ##     Col <- which(letters == strsplit(square, "")[[1]][1])
    ##     Row <- as.numeric(strsplit(square, "")[[1]][2])
    ##     KRange <- function(center = 5) {
    ##         K <- (center - 1):(center + 1)
    ##         K[0 < K & K <= 8]
    ##     }
    ##     Path <- expand.grid(letters[KRange(Col)], KRange(Row))
    ##     Path <- paste(Path[, 1], Path[, 2], sep = "")
    ##     Path[Path != square]
    ##     Test <- function(toSquare, fromSquare = square) {
    ##         fromColor <- pieceColor(fromSquare, game_pgn)
    ##         if (!is.na(fromColor)) {
    ##             if (fromColor == chesspatterns$white) {
    ##                 colorPattern <- chesspatterns$black
    ##             }
    ##             if (fromColor == chesspatterns$black) {
    ##                 colorPattern <- chesspatterns$white
    ##             }
    ##         }
    ##         else {
    ##             colorPattern <- chesspatterns$black
    ##         }
    ##         toColor <- pieceColor(toSquare, game_pgn)
    ##         grepl(colorPattern, position[game_pgn, toSquare]) | is.na(position[game_pgn, 
    ##             toSquare]) | grepl("phantom", position[game_pgn, 
    ##             toSquare])
    ##     }
    ##     Path <- Path[unlist(lapply(Path, Test))]
    ##     Path[Path != square]
    ## }
    ## 
    ## [[9]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[10]]
    ## [[10]]$value
    ## function (square, game_pgn = nrow(position)) 
    ## {
    ##     if (!grepl(chesspatterns$knight, position[game_pgn, square])) {
    ##         stop("this piece is not a knight")
    ##     }
    ##     Col <- which(letters == strsplit(square, "")[[1]][1])
    ##     Row <- as.numeric(strsplit(square, "")[[1]][2])
    ##     NRange <- function(center, dist) {
    ##         dist <- unique(c(dist, dist * -1))
    ##         N <- expand.grid(center, dist)
    ##         N <- apply(N, 1, sum)
    ##         N[0 < N & N <= 8]
    ##     }
    ##     Path <- rbind(expand.grid(letters[NRange(Col, 1)], NRange(Row, 
    ##         2)), expand.grid(letters[NRange(Col, 2)], NRange(Row, 
    ##         1)))
    ##     Path <- paste(Path[, 1], Path[, 2], sep = "")
    ##     Test <- function(toSquare, fromSquare = square) {
    ##         testPattern <- ifelse(grepl(chesspatterns$black, position[game_pgn, 
    ##             fromSquare]), chesspatterns$white, chesspatterns$black)
    ##         testPattern <- paste(testPattern, "phantom", sep = "|")
    ##         grepl(testPattern, position[game_pgn, toSquare]) | is.na(position[game_pgn, 
    ##             toSquare])
    ##     }
    ##     Path[unlist(lapply(Path, Test))]
    ## }
    ## 
    ## [[10]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[11]]
    ## [[11]]$value
    ## function (pgnLines) 
    ## {
    ##     pgn <- Lines[grep("\\d\\..+[abcdefgh]\\d", Lines)]
    ##     pgn <- paste(pgn, collapse = " ")
    ##     pgn <- strsplit(pgn, " ")[[1]]
    ##     pgn <- pgn[grep("[[:alpha:]]", pgn)]
    ##     pgn[seq(2, length(pgn), 2)] <- paste(seq(1, length(pgn)/2), 
    ##         "...", pgn[seq(2, length(pgn), 2)], sep = "")
    ##     pgn
    ## }
    ## 
    ## [[11]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[12]]
    ## [[12]]$value
    ## function (link, path = "login") 
    ## {
    ##     user = readline("username:")
    ##     pass = readline("password:")
    ##     hnd <- paste(unlist(strsplit(link, "/")[[1]][1:3]), collapse = "/")
    ##     print(hnd)
    ##     response <- POST(handle = hnd, path = path, body = list(login = user, 
    ##         pass = pass, redirect_url = link))
    ##     response
    ## }
    ## 
    ## [[12]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[13]]
    ## [[13]]$value
    ## function (game_pgn = nrow(position)) 
    ## {
    ##     square <- strsplit(row.names(position)[game_pgn], "\\.|_")[[1]]
    ##     square <- square[length(square)]
    ##     square <- gsub("[[:punct:]]", "", square)
    ##     column <- strsplit(square, "")[[1]]
    ##     row <- column[grepl("\\d", column)]
    ##     column <- column[grepl("[abcdefgh]", column)]
    ##     if (row == 5) {
    ##         fromRow <- 7
    ##         midRow <- 6
    ##     }
    ##     if (row == 4) {
    ##         fromRow <- 2
    ##         midRow <- 3
    ##     }
    ##     fromSquare <- paste(column, fromRow, sep = "")
    ##     midSquare <- paste(column, midRow, sep = "")
    ##     if (!row %in% c(4, 5)) {
    ##         return(F)
    ##     }
    ##     else {
    ##         isPawn <- grepl(chesspatterns$pawn, row.names(position)[game_pgn])
    ##         didntCapture <- !grepl("x", row.names(position)[game_pgn])
    ##         leapTo <- grepl("[abcdefg][45]", row.names(position)[game_pgn])
    ##         leapFrom <- position[game_pgn, square] == position[lookBack(game_pgn, 
    ##             1), fromSquare]
    ##         midVacant <- is.na(position[lookBack(game_pgn, 1), midSquare])
    ##         return(isPawn & didntCapture & leapTo & leapFrom & midVacant)
    ##     }
    ## }
    ## 
    ## [[13]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[14]]
    ## [[14]]$value
    ## function (ids, Var = "Event", workdir = "C:/Users/Josh/Documents") 
    ## {
    ##     if (tolower(Var) == "event") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$Event)
    ##         }
    ##     }
    ##     if (tolower(Var) == "site") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$Site)
    ##         }
    ##     }
    ##     if (tolower(Var) == "date") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$Date)
    ##         }
    ##     }
    ##     if (tolower(Var) == "white") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$White)
    ##         }
    ##     }
    ##     if (tolower(Var) == "black") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$Black)
    ##         }
    ##     }
    ##     if (tolower(Var) == "result") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$Result)
    ##         }
    ##     }
    ##     if (tolower(Var) == "whiteelo") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$WhiteElo)
    ##         }
    ##     }
    ##     if (tolower(Var) == "blackelo") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$BlackElo)
    ##         }
    ##     }
    ##     if (tolower(Var) == "timecontrol") {
    ##         FUN <- function(id) {
    ##             as.vector(read.meta(id, workdir)$TimeControl)
    ##         }
    ##     }
    ##     unlist(lapply(ids, FUN))
    ## }
    ## 
    ## [[14]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[15]]
    ## [[15]]$value
    ## function (mobilePattern = "total", DF = mobility, Legend = T, 
    ##     legendX = "topright") 
    ## {
    ##     mobileCol <- grep(mobilePattern, colnames(DF))
    ##     plot(as.ts(DF[, mobileCol[1]]), col = "red", ylim = c(0, 
    ##         max(c(DF[, mobileCol[1]], DF[, mobileCol[2]]))), ylab = "mobility", 
    ##         xlab = paste(unique(gsub("white|black", "", colnames(DF)[mobileCol])), 
    ##             collapse = ","))
    ##     lines(as.ts(DF[, mobileCol[2]]), col = "blue")
    ##     if (Legend) {
    ##         legend(legendX, legend = c("white", "black"), fill = c("red", 
    ##             "blue"))
    ##     }
    ## }
    ## 
    ## [[15]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[16]]
    ## [[16]]$value
    ## function (game_pgn) 
    ## {
    ##     DF <- pieceCount(game_pgn)
    ##     for (n in 1:nrow(DF)) {
    ##         options1 <- piecePost.(as.character(DF$square[n]), game_pgn)
    ##         DF[n, "options"] <- paste(options1, collapse = ";")
    ##         DF[n, "count"] <- ifelse(grepl(".\\d", DF[n, "options"]), 
    ##             length(options1), 0)
    ##     }
    ##     DF
    ## }
    ## 
    ## [[16]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[17]]
    ## [[17]]$value
    ## function (game_pgn = 2, piecePattern = "") 
    ## {
    ##     DF <- mobilityMap(game_pgn)
    ##     DF <- DF[grepl(piecePattern, DF$piece), ]
    ##     if (class(game_pgn) %in% c("numeric", "integer")) {
    ##         game_pgn <- row.names(position)[game_pgn]
    ##     }
    ##     data.frame(white = sum(DF[grepl(chesspatterns$white, DF[, 
    ##         "piece"]), "count"]), black = sum(DF[grepl(chesspatterns$black, 
    ##         DF[, "piece"]), "count"]), row.names = game_pgn)
    ## }
    ## 
    ## [[17]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[18]]
    ## [[18]]$value
    ## function (row, df = echess, dbN = "local", h = "localhost") 
    ## {
    ##     library(RMongo)
    ##     doc <- paste("{", "'_id':'", paste(df[row, 1], df[row, 2], 
    ##         df[row, 3], sep = "_"), "','", colnames(df)[1], "':'", 
    ##         df[row, 1], "','", colnames(df)[2], "':'", df[row, 2], 
    ##         "','", colnames(df)[3], "':'", df[row, 3], "','", colnames(df)[4], 
    ##         "':'", df[row, 4], "','", colnames(df)[5], "':'", df[row, 
    ##             5], "','", colnames(df)[6], "':'", df[row, 6], "','", 
    ##         colnames(df)[7], "':'", df[row, 7], "','", colnames(df)[8], 
    ##         "':'", df[row, 8], "','", colnames(df)[9], "':'", df[row, 
    ##             9], "'}", sep = "")
    ##     mongo <- mongoDbConnect(dbName = dbN, host = h)
    ##     output <- dbInsertDocument(rmongo.object = mongo, collection = "josh_chess", 
    ##         doc = doc)
    ##     dbDisconnect(mongo)
    ## }
    ## 
    ## [[18]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[19]]
    ## [[19]]$value
    ## function (game_pgn = nrow(position)) 
    ## {
    ##     square <- strsplit(row.names(position)[game_pgn], "\\.|_")[[1]]
    ##     square <- square[length(square)]
    ##     square <- gsub("[[:punct:]]", "", square)
    ##     square <- strsplit(square, "")[[1]]
    ##     square <- paste(square[length(square) - (1:0)], collapse = "")
    ##     piece <- position[game_pgn, square]
    ##     Column <- strsplit(square, "")[[1]]
    ##     Row <- Column[grepl("\\d", Column)]
    ##     Column <- Column[grepl("[abcdefgh]", Column)]
    ##     neighborCol <- letters[which(letters == Column) + c(-1, 1)]
    ##     neighborSquare <- paste(neighborCol, Row, sep = "")
    ##     neighborPiece <- position[game_pgn, neighborSquare]
    ##     arePawns <- grepl("pawn", piece) & grepl("pawn", neighborPiece)
    ##     diffColors <- (grepl(chesspatterns$black, piece) & grepl(chesspatterns$white, 
    ##         neighborPiece)) | grepl(chesspatterns$white, piece) & 
    ##         grepl(chesspatterns$black, neighborPiece)
    ##     passantRows <- grepl("[45]", square) & grepl("[45]", neighborSquare)
    ##     ifelse(F %in% c(arePawns, diffColors, passantRows), return(F), 
    ##         return(T))
    ## }
    ## 
    ## [[19]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[20]]
    ## [[20]]$value
    ## function (new_pgn, startPosition = nrow(position)) 
    ## {
    ##     castle <- grepl(chesspatterns$castle, new_pgn)
    ##     promotion <- grepl("[QRBN][+#]?$", new_pgn)
    ##     color <- ifelse(grepl(chesspatterns$black, new_pgn), "black", 
    ##         "white")
    ##     move <- strsplit(new_pgn, "\\.|_")[[1]]
    ##     move <- as.numeric(move[grep("^\\d+$", move)])
    ##     move <- move[length(move)]
    ##     if (!castle & !promotion) {
    ##         piece <- paste(color, pgnPiece(new_pgn))
    ##         endSquare <- gsub("[[:punct:]]$", "", new_pgn)
    ##         endSquare <- strsplit(endSquare, "")[[1]]
    ##         endSquare <- paste(endSquare[(length(endSquare) - 1):length(endSquare)], 
    ##             collapse = "")
    ##         workFrame <- pieceCount(startPosition)
    ##         workFrame <- workFrame[grepl(color, workFrame$piece), 
    ##             ]
    ##         workFrame <- workFrame[grepl(tolower(piece), tolower(as.vector(workFrame$piece))), 
    ##             ]
    ##         workList <- lapply(as.character(workFrame$square), piecePost., 
    ##             game_pgn = startPosition)
    ##         names(workList) <- as.character(workFrame$square)
    ##         TEST <- function(n) {
    ##             endSquare %in% workList[[n]]
    ##         }
    ##         startSquare <- which(unlist(lapply(1:length(workList), 
    ##             TEST)))
    ##         if (1 < length(startSquare)) {
    ##             fromPattern <- gsub(endSquare, "", new_pgn)
    ##             fromPattern <- tail(strsplit(fromPattern, "\\.")[[1]], 
    ##                 1)
    ##             fromPattern <- gsub("[[:upper:]]|[[:punct:]]|x", 
    ##                 "", fromPattern)
    ##             startSquare <- startSquare[grepl(fromPattern, names(workList)[startSquare])]
    ##         }
    ##         startSquare <- names(workList)[[startSquare]]
    ##         newPosition <- position[startPosition, ]
    ##         row.names(newPosition) <- new_pgn
    ##         newPosition[, startSquare] <- NA
    ##         newPosition[, endSquare] <- piece
    ##         if (grepl(chesspatterns$pawn, piece) & grepl("phantom", 
    ##             position[startPosition, endSquare])) {
    ##             Col <- which(letters == strsplit(endSquare, "")[[1]][1])
    ##             if (grepl("^.6$", endSquare)) {
    ##                 concretePawn <- paste(letters[Col], "5", sep = "")
    ##             }
    ##             if (grepl("^.3$", endSquare)) {
    ##                 concretePawn <- paste(letters[Col], "4", sep = "")
    ##             }
    ##             newPosition[, concretePawn] <- NA
    ##         }
    ##         phanP <- as.vector(apply(newPosition, 2, grepl, pattern = "phantom"))
    ##         newPosition[, phanP] <- NA
    ##         if (grepl(chesspatterns$pawn, piece) & grepl("2|7", startSquare) & 
    ##             grepl("4|5", endSquare)) {
    ##             Col <- which(letters == strsplit(startSquare, "")[[1]][1])
    ##             if (color == "black") {
    ##                 phantomSquare <- paste(letters[Col], "6", sep = "")
    ##             }
    ##             if (color == "white") {
    ##                 phantomSquare <- paste(letters[Col], "3", sep = "")
    ##             }
    ##             newPosition[, phantomSquare] <- paste("phantom", 
    ##                 color, "pawn")
    ##         }
    ##     }
    ##     if (castle) {
    ##         piece <- paste(chesspatterns$king, chesspatterns$rook, 
    ##             sep = "|")
    ##         endSquare <- unlist(CSTL(new_pgn)[c("king", "rook")])
    ##         startSquare <- unlist(CSTL(new_pgn)[c("kingVacant", "rookVacant")])
    ##         newPosition <- position[startPosition, ]
    ##         row.names(newPosition) <- new_pgn
    ##         newPosition[, startSquare] <- NA
    ##         newPosition[, endSquare] <- paste(color, names(endSquare))
    ##     }
    ##     if (promotion) {
    ##         piece <- paste(color, pgnPiece(new_pgn))
    ##         endSquare <- strsplit(new_pgn, "[_\\.]")[[1]]
    ##         if (grepl("_", new_pgn)) {
    ##             endSquare <- endSquare[-1]
    ##         }
    ##         endSquare <- endSquare[grepl("[[:lower:]]\\d=?[QRBN].?$", 
    ##             endSquare)]
    ##         endSquare <- gsub("[[:upper:]]|[[:punct:]]", "", endSquare)
    ##         endSquare <- paste(tail(strsplit(endSquare, "")[[1]], 
    ##             2), collapse = "")
    ##         workFrame <- pieceCount(startPosition)
    ##         workFrame <- workFrame[grepl(color, workFrame$piece), 
    ##             ]
    ##         workFrame <- workFrame[grepl("pawn", tolower(workFrame$piece)), 
    ##             ]
    ##         workList <- lapply(as.character(workFrame$square), piecePost., 
    ##             game_pgn = startPosition)
    ##         names(workList) <- as.character(workFrame$square)
    ##         TEST <- function(n) {
    ##             endSquare %in% workList[[n]]
    ##         }
    ##         startSquare <- which(unlist(lapply(1:length(workList), 
    ##             TEST)))
    ##         if (1 < length(startSquare)) {
    ##             fromPattern <- strsplit(new_pgn, "=")[[1]][1]
    ##             fromPattern <- gsub(endSquare, "", new_pgn)
    ##             fromPattern <- tail(strsplit(fromPattern, "\\.")[[1]], 
    ##                 1)
    ##             fromPattern <- gsub("[[:upper:]]|[[:punct:]]|x", 
    ##                 "", fromPattern)
    ##             startSquare <- startSquare[grepl(fromPattern, names(workList)[startSquare])]
    ##         }
    ##         startSquare <- names(workList)[[startSquare]]
    ##         newPosition <- position[startPosition, ]
    ##         row.names(newPosition) <- new_pgn
    ##         newPosition[, startSquare] <- NA
    ##         newPosition[, endSquare] <- piece
    ##     }
    ##     newPosition
    ## }
    ## 
    ## [[20]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[21]]
    ## [[21]]$value
    ## function (piece, square) 
    ## {
    ##     Col <- which(letters == strsplit(square, "")[[1]][1])
    ##     Row <- as.numeric(strsplit(square, "")[[1]][2])
    ##     if (grepl(chesspatterns$pawn, piece)) {
    ##         if (!exists("color")) {
    ##             color <- ""
    ##         }
    ##         if (grepl(chesspatterns$black, piece)) {
    ##             direction <- -1
    ##         }
    ##         if (grepl(chesspatterns$white, piece)) {
    ##             direction <- 1
    ##         }
    ##         PRange <- function(center) {
    ##             P <- c((center - 1), (center + 1))
    ##             P[0 < P & P <= 8]
    ##         }
    ##         AttSq <- paste(letters[PRange(Col)], Row + direction, 
    ##             sep = "")
    ##         ifelse((direction == -1 & Row == 7) | (direction == 1 & 
    ##             Row == 2), distance <- 1:2, distance <- 1)
    ##         Path <- paste(letters[Col], Row + (distance * direction), 
    ##             sep = "")
    ##         Path <- c(Path, AttSq)
    ##     }
    ##     if (grepl(chesspatterns$bishop, piece)) {
    ##         B <- function(x) {
    ##             V <- c(x + Col - Row, Col + Row - x)
    ##             paste(letters[x], V[0 < V & V <= 8], sep = "")
    ##         }
    ##         Path <- unique(unlist(sapply(1:8, B)))
    ##     }
    ##     if (grepl(chesspatterns$knight, piece)) {
    ##         NRange <- function(center, dist) {
    ##             dist <- unique(c(dist, dist * -1))
    ##             N <- expand.grid(center, dist)
    ##             N <- apply(N, 1, sum)
    ##             N[0 < N & N <= 8]
    ##         }
    ##         Path <- rbind(expand.grid(letters[NRange(Col, 1)], NRange(Row, 
    ##             2)), expand.grid(letters[NRange(Col, 2)], NRange(Row, 
    ##             1)))
    ##         Path <- paste(Path[, 1], Path[, 2], sep = "")
    ##     }
    ##     if (grepl(chesspatterns$rook, piece)) {
    ##         Path <- unique(c(paste(letters[Col], 1:8, sep = ""), 
    ##             paste(letters[1:8], Row, sep = "")))
    ##     }
    ##     if (grepl(chesspatterns$queen, piece)) {
    ##         B <- function(x) {
    ##             V <- c(x + Col - Row, Col + Row - x)
    ##             paste(letters[x], V[0 < V & V <= 8], sep = "")
    ##         }
    ##         Path <- c(unique(unlist(sapply(1:8, B))), unique(c(paste(letters[Col], 
    ##             1:8, sep = ""), paste(letters[1:8], Row, sep = ""))))
    ##     }
    ##     if (grepl(chesspatterns$king, piece)) {
    ##         KRange <- function(center) {
    ##             K <- (center - 1):(center + 1)
    ##             K[0 < K & K <= 8]
    ##         }
    ##         Path <- expand.grid(letters[KRange(Col)], KRange(Row))
    ##         Path <- paste(Path[, 1], Path[, 2], sep = "")
    ##     }
    ##     Path[Path != square]
    ## }
    ## 
    ## [[21]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[22]]
    ## [[22]]$value
    ## function (game_pgn = nrow(position)) 
    ## {
    ##     square <- strsplit(row.names(position)[game_pgn], "\\.|_")[[1]]
    ##     square <- square[length(square)]
    ##     square <- gsub("[[:punct:]]", "", square)
    ##     column <- strsplit(square, "")[[1]]
    ##     row <- column[grepl("\\d", column)]
    ##     column <- column[grepl("[abcdefgh]", column)]
    ##     if (row == 5) {
    ##         fromRow <- 7
    ##         midRow <- 6
    ##     }
    ##     if (row == 4) {
    ##         fromRow <- 2
    ##         midRow <- 3
    ##     }
    ##     fromSquare <- paste(column, fromRow, sep = "")
    ##     midSquare <- paste(column, midRow, sep = "")
    ##     if (!row %in% c(4, 5)) {
    ##         return(F)
    ##     }
    ##     else {
    ##         isPawn <- grepl(chesspatterns$pawn, row.names(position)[game_pgn])
    ##         didntCapture <- !grepl("x", row.names(position)[game_pgn])
    ##         leapTo <- grepl("[abcdefg][45]", row.names(position)[game_pgn])
    ##         leapFrom <- position[game_pgn, square] == position[lookBack(game_pgn, 
    ##             1), fromSquare]
    ##         midVacant <- is.na(position[lookBack(game_pgn, 1), midSquare])
    ##         return(isPawn & didntCapture & leapTo & leapFrom & midVacant)
    ##     }
    ## }
    ## 
    ## [[22]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[23]]
    ## [[23]]$value
    ## function (square, game_pgn = nrow(position)) 
    ## {
    ##     if (grepl(chesspatterns$black, position[game_pgn, square])) {
    ##         direction <- -1
    ##     }
    ##     if (grepl(chesspatterns$white, position[game_pgn, square])) {
    ##         direction <- 1
    ##     }
    ##     if (!grepl(paste(chesspatterns$black, chesspatterns$white, 
    ##         sep = "|"), position[game_pgn, square])) {
    ##         direction <- c(1, -1)
    ##     }
    ##     Col <- which(letters == strsplit(square, "")[[1]][1])
    ##     Row <- as.numeric(strsplit(square, "")[[1]][2])
    ##     AttRange <- c((Col - 1), (Col + 1))
    ##     AttRange <- AttRange[0 < AttRange & AttRange <= 8]
    ##     AttSq <- paste(letters[AttRange], Row + direction, sep = "")
    ##     ifelse((direction == -1 & Row == 7) | (direction == 1 & Row == 
    ##         2), distance <- 1:2, distance <- 1)
    ##     normSq <- paste(letters[Col], Row + (distance * direction), 
    ##         sep = "")
    ##     AttTest <- function(AttSquare, fromSquare = square) {
    ##         ifelse((grepl(chesspatterns$black, position[game_pgn, 
    ##             fromSquare]) & grepl(chesspatterns$white, position[game_pgn, 
    ##             AttSquare])) | (grepl(chesspatterns$white, position[game_pgn, 
    ##             fromSquare]) & grepl(chesspatterns$black, position[game_pgn, 
    ##             AttSquare])), T, F)
    ##     }
    ##     normTest <- function(normSquare, fromSquare = square) {
    ##         DF <- betweens(normSquare, fromSquare, game_pgn)
    ##         DF <- DF[DF$squares != fromSquare, ]
    ##         clear <- unlist(lapply(DF$occupants, is.na))
    ##         sum(clear) == nrow(DF)
    ##     }
    ##     AttSq <- AttSq[unlist(lapply(AttSq, AttTest))]
    ##     normSq <- normSq[unlist(lapply(normSq, normTest))]
    ##     ifelse(0 < length(c(normSq, AttSq)), return(c(normSq, AttSq)), 
    ##         return(NA))
    ## }
    ## 
    ## [[23]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[24]]
    ## [[24]]$value
    ## [[24]]$value$bishop
    ## [1] "[Bb][Ii][Ss][Hh][Oo][Pp]|(^\\d+\\.+Bx?([[:lower:]]\\d)+$)"
    ## 
    ## [[24]]$value$black
    ## [1] "[Bb][Ll][Aa][Cc][Kk]|\\.{3}"
    ## 
    ## [[24]]$value$capture
    ## [1] "x\\[[:lower:]]\\d"
    ## 
    ## [[24]]$value$castle
    ## [1] "[Cc][Aa][Ss][Tt][Ll][Ee]|O-O"
    ## 
    ## [[24]]$value$castleQ
    ## [1] "[Cc][Aa][Ss][Tt][Ll][Ee]|O-O-O"
    ## 
    ## [[24]]$value$checkmate
    ## [1] "#"
    ## 
    ## [[24]]$value$check
    ## [1] "+"
    ## 
    ## [[24]]$value$king
    ## [1] "[Kk][Ii][Nn][Gg]|(^\\d+\\.+Kx?([[:lower:]]\\d)+$)"
    ## 
    ## [[24]]$value$knight
    ## [1] "[Kk][Nn][Ii][Gg][Hh][Tt]|(\\d+\\.+N([[:lower:]]|\\d)?x?[[:lower:]]\\d(#|\\+)?$)"
    ## 
    ## [[24]]$value$pawn
    ## [1] "[Pp][Aa][Ww][Nn]|(\\d+\\.+[[:lower:]]?x?([[:lower:]]\\d)+$)"
    ## 
    ## [[24]]$value$pgn
    ## [1] "\\d\\..*[abcdefgh].*[12345678]"
    ## 
    ## [[24]]$value$queen
    ## [1] "[Qq][Uu][Ee][Ee][Nn]|(^\\d+\\.+Qx?([[:lower:]]\\d)+[[:punct:]]+?$)"
    ## 
    ## [[24]]$value$rook
    ## [1] "[Rr][Oo][Oo][Kk]|(^\\d+\\.+Rx?([[:lower:]]\\d)+$)"
    ## 
    ## [[24]]$value$white
    ## [1] "[Ww][Hh][Ii][Tt][Ee]|([[:alnum:]]\\.{1,1}[[:alpha:]])"
    ## 
    ## 
    ## [[24]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[25]]
    ## [[25]]$value
    ## function (pgn) 
    ## {
    ##     test <- function(L) {
    ##         grepl(L, pgn)
    ##     }
    ##     P <- c("K", "Q", "R", "N", "B")
    ##     Piece = c("King", "Queen", "Rook", "Knight", "Bishop")
    ##     df <- data.frame(P = P, Piece = Piece, assess = unlist(lapply(P, 
    ##         test)))
    ##     if (sum(df$assess) == 0) {
    ##         return("pawn")
    ##     }
    ##     if (sum(df$assess) == 1) {
    ##         return(as.character(subset(df$Piece, df$assess == TRUE)))
    ##     }
    ##     if (1 < sum(df$assess)) {
    ##         stop("Input contains extra piece")
    ##     }
    ## }
    ## 
    ## [[25]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[26]]
    ## [[26]]$value
    ## function (square = "a1", game_pgn = 2) 
    ## {
    ##     patternVec <- c(chesspatterns$black, chesspatterns$white)
    ##     patternTest <- unlist(lapply(patternVec, grepl, x = position[game_pgn, 
    ##         square]))
    ##     ifelse(sum(patternTest) == 0, return(NA), return(patternVec[patternTest]))
    ## }
    ## 
    ## [[26]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[27]]
    ## [[27]]$value
    ## function (game_pgn = nrow(position)) 
    ## {
    ##     Col <- !apply(position[game_pgn, ], 2, is.na)
    ##     if (!sum(Col) == 1) {
    ##         square <- colnames(position)[Col]
    ##         piece <- as.character(as.matrix(position[game_pgn, Col]))
    ##         DF <- data.frame(square = colnames(position[game_pgn, 
    ##             Col]), piece = as.character(as.matrix(position[game_pgn, 
    ##             Col])), stringsAsFactors = F)
    ##     }
    ##     else {
    ##         DF <- data.frame(square = colnames(position[game_pgn, 
    ##             Col]), piece = piece, stringsAsFactors = F)
    ##     }
    ##     DF
    ## }
    ## 
    ## [[27]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[28]]
    ## [[28]]$value
    ## function (square, game_pgn) 
    ## {
    ##     piece <- as.vector(position[game_pgn, square])
    ##     if (grepl(chesspatterns$pawn, piece)) {
    ##         return(pawnPost.(square, game_pgn))
    ##     }
    ##     if (grepl(chesspatterns$knight, piece)) {
    ##         return(paste("", knightPost.(square, game_pgn), sep = ""))
    ##     }
    ##     if (grepl(chesspatterns$bishop, piece)) {
    ##         return(paste("", bishopPost.(square, game_pgn), sep = ""))
    ##     }
    ##     if (grepl(chesspatterns$rook, piece)) {
    ##         return(paste("", rookPost.(square, game_pgn), sep = ""))
    ##     }
    ##     if (grepl(chesspatterns$queen, piece)) {
    ##         return(paste("", queenPost.(square, game_pgn), sep = ""))
    ##     }
    ##     if (grepl(chesspatterns$king, piece)) {
    ##         return(paste("", kingPost.(square, game_pgn), sep = ""))
    ##     }
    ## }
    ## 
    ## [[28]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[29]]
    ## [[29]]$value
    ## function (square, game_pgn) 
    ## {
    ##     R <- rookPost.(square, game_pgn)
    ##     B <- bishopPost.(square, game_pgn)
    ##     sort(c(R, B))
    ## }
    ## 
    ## [[29]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[30]]
    ## [[30]]$value
    ## function (pgn) 
    ## {
    ##     lastRow <- row.names(position)[nrow(position)]
    ##     if (grepl("zero", lastRow)) {
    ##         move <- 1
    ##     }
    ##     else {
    ##         move <- strsplit(lastRow, "_|\\.")[[1]]
    ##         move <- as.numeric(move[grepl("^\\d+$", move)])
    ##         if (grepl(chesspatterns$black, lastRow)) {
    ##             move <- move + 1
    ##         }
    ##     }
    ##     newRow <- paste(gameName, "_", move, pgn, sep = "")
    ##     position <- rbind(position, newPosition(newRow, nrow(position)))
    ##     return(position)
    ## }
    ## 
    ## [[30]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[31]]
    ## [[31]]$value
    ## function (pgnFile) 
    ## {
    ##     if (grepl("chess.com", pgnFile)) {
    ##         if (!grepl("@", pgnFile)) {
    ##             pgnFile <- paste("https://thinkboolean:witstorm666", 
    ##                 gsub("https://", "", pgnFile), sep = "@")
    ##         }
    ##         if (!grepl("livechess", pgnFile)) {
    ##             con = url(pgnFile)
    ##             htmlCode = readLines(con)
    ##             close(con)
    ##             pgn <- unlist(strsplit(htmlCode[grep("\\d\\.[[:lower:]]\\d", 
    ##                 htmlCode)], "Board\\(\\'|\\'\\)"))
    ##             pgn <- pgn[grepl("\\d\\.[[:lower:]]\\d", pgn)]
    ##             pgn <- strsplit(pgn, "\\+(?!\\+)", perl = T)[[1]]
    ##             Title <- htmlCode[grep("<title>", htmlCode, ignore.case = T)]
    ##             Title <- gsub("</?title>| {2}", "", Title)
    ##             metaChess <- data.frame(Title, pgn = paste(pgn, collapse = " "))
    ##         }
    ##         else {
    ##             message("cannot currently extract live games")
    ##         }
    ##     }
    ##     else {
    ##         pgnLines <- readLines(pgnFile)
    ##         notationLines <- (grep("\\d\\.[KQRNB]?[abcdefgh12345678]?x?[abcdefgh][1234567]", 
    ##             pgnLines))
    ##         range <- data.frame(start = grep("\\[Event", pgnLines))
    ##         range <- cbind(range, stop = c(range[-1, "start"] - 1, 
    ##             length(pgnLines)))
    ##         for (n in 1:nrow(range)) {
    ##             m <- seq(range[n, "start"], range[n, "stop"])
    ##             meta <- pgnLines[m][grep("\\[", pgnLines[m])]
    ##             pgn <- paste(pgnLines[m][!pgnLines[m] %in% meta], 
    ##                 collapse = " ")
    ##             pgn <- gsub(" {2,}", " ", pgn)
    ##             meta <- unlist(strsplit(meta, "\""))
    ##             gsub("\\[| $", "", meta[grep("^\\[", meta)])
    ##             DF <- data.frame(matrix(meta[-grep("\\[|\\]", meta)], 
    ##                 nrow = 1))
    ##             colnames(DF) <- gsub("\\[| $", "", meta[grep("^\\[", 
    ##                 meta)])
    ##             DF <- cbind(DF, pgn = pgn)
    ##             if (n == 1) {
    ##                 metaChess <- DF
    ##             }
    ##             else {
    ##                 DF <- DF[, colnames(DF) %in% colnames(metaChess)]
    ##                 metaChess <- rbind(metaChess, DF)
    ##             }
    ##         }
    ##     }
    ##     metaChess
    ## }
    ## 
    ## [[31]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[32]]
    ## [[32]]$value
    ## function (Link, type, errorMessage = "unfound") 
    ## {
    ##     Html <- tryCatch(readLines(Link, warn = F), error = function(e) {
    ##         return(errorMessage)
    ##     })
    ##     STR <- Html[grepl("vs", Html)]
    ##     STR <- unlist(strsplit(STR, "\""))
    ##     STR <- STR[grepl("vs", STR)]
    ##     STR <- STR[grepl("(\\(\\d+\\).+){2}", STR)]
    ##     STR <- unique(STR)
    ##     if (length(STR) == 0) {
    ##         STR <- "0"
    ##     }
    ##     if (STR == "0" & type %in% c("correspondence", "live")) {
    ##         return(data.frame(White = "", Black = "", stringsAsFactors = F))
    ##     }
    ##     else {
    ##         if (type == "live") {
    ##             Time <- Html[grep("Time:$", Html) + 1]
    ##         }
    ##         if (type == "correspondence") {
    ##             Time <- unlist(strsplit(STR, "[[:punct:]]"))
    ##             Time <- Time[grepl("over", Time)]
    ##             Time <- strsplit(Time, "over ")[[1]][2]
    ##         }
    ##         if (grepl("live|corres", type)) {
    ##             White <- strsplit(STR, " vs.? ")[[1]][1]
    ##             WhiteElo <- unlist(strsplit(White, "[[:punct:]]"))
    ##             WhiteElo <- gsub(" ", "", WhiteElo[grepl("^\\d+$", 
    ##                 WhiteElo)])
    ##             if (length(WhiteElo) == 0) {
    ##                 WhiteElo <- 0
    ##             }
    ##             White <- strsplit(White, "[[:blank:]]")[[1]][1]
    ##             Black <- strsplit(STR, " vs |\\.")[[1]][2]
    ##             BlackElo <- unlist(strsplit(Black, "[[:punct:]]"))
    ##             BlackElo <- gsub(" ", "", BlackElo[grepl("^\\d+$", 
    ##                 BlackElo)])
    ##             Black <- unlist(strsplit(Black, "[[:blank:]]"))
    ##             if (length(BlackElo) == 0) {
    ##                 BlackElo <- 0
    ##             }
    ##             Black <- Black[grepl(".", Black)][1]
    ##             winner <- unlist(strsplit(STR, "[[:punct:]]"))
    ##             winner <- winner[grepl("won", winner)]
    ##             if (length(winner) == 0) {
    ##                 winner = ""
    ##             }
    ##             else {
    ##                 winner <- gsub(" ", "", strsplit(winner, "won")[[1]][1])
    ##             }
    ##             if (winner == White) {
    ##                 Result <- "1-0"
    ##             }
    ##             if (winner == Black) {
    ##                 Result <- "0-1"
    ##             }
    ##             if (grepl("draw", STR)) {
    ##                 Result <- "draw"
    ##             }
    ##             if (grepl("stalemate", STR)) {
    ##                 Result <- "stalemate"
    ##             }
    ##             if (!exists("Result")) {
    ##                 Result <- ""
    ##             }
    ##             if (grepl("draw", STR)) {
    ##                 Termination <- "draw"
    ##             }
    ##             if (grepl("won on time", STR)) {
    ##                 Termination <- "time"
    ##             }
    ##             if (grepl("won by checkmate", STR)) {
    ##                 Termination <- "checkmate"
    ##             }
    ##             if (grepl("won by resignation", STR)) {
    ##                 Termination <- "resignation"
    ##             }
    ##             if (grepl("game abandoned", STR)) {
    ##                 Termination <- "game abandoned"
    ##             }
    ##             if (!grepl("draw|on time|by checkmate|by resignation|game abandoned", 
    ##                 STR)) {
    ##                 stop("new termination test required")
    ##                 print(STR)
    ##             }
    ##             Moves <- unlist(strsplit(STR, "in|by|over|\\."))
    ##             Moves <- Moves[grepl("\\d moves", Moves)][1]
    ##             Moves <- gsub("[[:alpha:]]|[[:blank:]]", "", Moves)
    ##             return(data.frame(White, Black, Result, WhiteElo, 
    ##                 BlackElo, Time, Moves, Termination, stringsAsFactors = F))
    ##         }
    ##     }
    ##     if (type == "open") {
    ##         chesspatterns$pgn <- "\\d\\.[[:blank:]]?([[:alnum:]])?[KQRNB]?x?[abcdefgh]\\d"
    ##         pgn <- Html[grep(chesspatterns$pgn, Html)]
    ##         if (length(pgn) != 0) {
    ##             pgn <- paste(pgn[grep("^1", pgn)[1]:length(pgn)], 
    ##                 collapse = " ")
    ##             pgn <- gsub("\\.[[:blank:]]", ".", pgn)
    ##         }
    ##         else {
    ##             pgn <- ""
    ##         }
    ##         info <- Html[grep("^\\[.+&quot.+\\]$", Html)]
    ##         info <- gsub("^\\[|&quot;]$", "", info)
    ##         info <- unlist(strsplit(info, " &quot"))
    ##         if (length(info)%%2 != 0) {
    ##             info <- info[-length(info)]
    ##         }
    ##         if (2 < length(info)) {
    ##             openDF <- info[seq(2, length(info), 2)]
    ##             openDF <- gsub(";", "", openDF)
    ##             openDF <- data.frame(matrix(openDF, nrow = 1), stringsAsFactors = F)
    ##             colnames(openDF) <- info[seq(1, length(info), 2)]
    ##             openDF <- cbind(openDF, pgn, stringsAsFactors = F)
    ##         }
    ##         else {
    ##             openDF <- data.frame(White = "", Black = "", stringsAsFactors = F)
    ##         }
    ##         return(openDF)
    ##     }
    ## }
    ## 
    ## [[32]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[33]]
    ## [[33]]$value
    ## function (square, game_pgn) 
    ## {
    ##     Col <- which(letters == strsplit(square, "")[[1]][1])
    ##     Row <- as.numeric(strsplit(square, "")[[1]][2])
    ##     Path <- unique(c(paste(letters[Col], 1:8, sep = ""), paste(letters[1:8], 
    ##         Row, sep = "")))
    ##     Path <- sort(Path[Path != square])
    ##     Test <- function(toSquare, fromSquare = square) {
    ##         fromColor <- pieceColor(fromSquare, game_pgn)
    ##         if (!is.na(fromColor)) {
    ##             if (fromColor == chesspatterns$white) {
    ##                 colorPattern <- chesspatterns$black
    ##             }
    ##             if (fromColor == chesspatterns$black) {
    ##                 colorPattern <- chesspatterns$white
    ##             }
    ##         }
    ##         else {
    ##             colorPattern <- chesspatterns$black
    ##         }
    ##         toColor <- pieceColor(toSquare, game_pgn)
    ##         if (grepl("phantom", position[game_pgn, toSquare])) {
    ##             toColor <- NA
    ##         }
    ##         DF <- betweens(toSquare, fromSquare, game_pgn)
    ##         T1 <- (sum(is.na(DF$occupants[-c(1, nrow(DF))]) | grepl("phantom", 
    ##             DF$occupants[-c(1, nrow(DF))])) == nrow(DF) - 2)
    ##         T2a <- grepl(colorPattern, DF[DF$squares == toSquare, 
    ##             "occupants"])
    ##         T2b <- is.na(DF[DF$squares == toSquare, "occupants"]) | 
    ##             grepl("phantom", DF[DF$squares == toSquare, "occupants"])
    ##         T1 & (T2a | T2b)
    ##     }
    ##     Path[unlist(lapply(Path, Test))]
    ## }
    ## 
    ## [[33]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[34]]
    ## [[34]]$value
    ## NULL
    ## 
    ## [[34]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[35]]
    ## [[35]]$value
    ## function (live.link = NULL, id = NULL) 
    ## {
    ##     if (length(id) != 1) {
    ##         id <- gsub("^.+/", "", live.link)
    ##     }
    ##     else {
    ##         live.link <- file.path("https://www.chess.com/live/game", 
    ##             id)
    ##     }
    ##     Html <- readLines(live.link)
    ##     if (sum(grepl(paste(id, ",&quot;", sep = ""), Html)) != 1) {
    ##         return(data.frame(White = "", Black = "", stringsAsFactors = F))
    ##     }
    ##     else {
    ##         Html <- Html[grepl(paste(id, ",&quot;", sep = ""), Html)]
    ##     }
    ##     Json <- gsub("&quot;", "\"", Html)
    ##     Json <- gsub("\r|\n|,$", "", Json)
    ##     Json <- jsonlite::fromJSON(Json)
    ##     pgn <- Json$pgn
    ##     Json$pgn <- NULL
    ##     DF1 <- matrix(Json, ncol = length(Json), dimnames = list(NULL, 
    ##         names(Json)))
    ##     DF1 <- data.frame(DF1, stringsAsFactors = F)
    ##     pgn <- strsplit(pgn, "\\r|\\n")[[1]]
    ##     pgn <- pgn[grepl(".", pgn)]
    ##     FEN0 <- pgn[grepl("FEN", pgn)]
    ##     DF2 <- pgn[grepl("^\\[.+\\]$", pgn)]
    ##     pgn <- paste(pgn[!grepl("^\\[", pgn)], collapse = "")
    ##     DF2 <- matrix(sub("^.+ ", "", DF2), nrow = 1, byrow = T, 
    ##         dimnames = list(NULL, gsub("\".+$", "", DF2)))
    ##     DF2 <- data.frame(DF2, stringsAsFactors = F)
    ##     DF2 <- apply(DF2, 2, gsub, pattern = "\\[|\\]|\\\"", replacement = "")
    ##     DF2 <- matrix(DF2, nrow = 1, byrow = T, dimnames = list(NULL, 
    ##         names(DF2)))
    ##     DF2 <- as.data.frame(DF2, stringsAsFactors = F)
    ##     colnames(DF2) <- gsub("[[:punct:]]|^X", "", colnames(DF2))
    ##     DF <- data.frame(cbind(DF1, DF2), stringsAsFactors = F)
    ##     rm(DF1, DF2)
    ##     for (m in 1:ncol(DF)) {
    ##         if (class(DF[, m]) == "list") {
    ##             DF[, m] <- as.character(DF[, m])
    ##         }
    ##     }
    ##     DF <- cbind(DF, pgn, FEN0, stringsAsFactors = F)
    ##     DF[, !grepl("^FEN$", colnames(DF))]
    ## }
    ## 
    ## [[35]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[36]]
    ## [[36]]$value
    ## function (gameName = "000", includeEmpty = TRUE) 
    ## {
    ##     position <- empty(gameName)
    ##     position <- rbind(empty(), empty())
    ##     row.names(position) <- paste(gameName, c("empty", "zero"), 
    ##         sep = "_")
    ##     for (Col in 1:ncol(position)) {
    ##         class(position[, Col]) <- "character"
    ##         if (grepl("(1|2)$", colnames(position)[Col])) {
    ##             position[2, Col] <- "white"
    ##         }
    ##         if (grepl("(7|8)$", colnames(position)[Col])) {
    ##             position[2, Col] <- "black"
    ##         }
    ##     }
    ##     for (Col in 1:ncol(position)) {
    ##         if (grepl("(7|2)$", colnames(position)[Col])) {
    ##             position[2, Col] <- paste(position[2, Col], "pawn")
    ##         }
    ##         if (grepl("(a|h)(1|8)", colnames(position)[Col])) {
    ##             position[2, Col] <- paste(position[2, Col], "Rook")
    ##         }
    ##         if (grepl("(b|g)(1|8)", colnames(position)[Col])) {
    ##             position[2, Col] <- paste(position[2, Col], "Knight")
    ##         }
    ##         if (grepl("(c|f)(1|8)", colnames(position)[Col])) {
    ##             position[2, Col] <- paste(position[2, Col], "Bishop")
    ##         }
    ##         if (grepl("d(1|8)", colnames(position)[Col])) {
    ##             position[2, Col] <- paste(position[2, Col], "Queen")
    ##         }
    ##         if (grepl("e(1|8)", colnames(position)[Col])) {
    ##             position[2, Col] <- paste(position[2, Col], "King")
    ##         }
    ##     }
    ##     if (includeEmpty == FALSE) {
    ##         position <- position[nrow(position), ]
    ##     }
    ##     position
    ## }
    ## 
    ## [[36]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[37]]
    ## [[37]]$value
    ## function (id = 1000, type = "correspondence") 
    ## {
    ##     if (type == "correspondence") {
    ##         Url <- "http://www.chess.com/echess/game?id="
    ##         Url <- paste(Url, id, sep = "")
    ##         L <- length(readLines(Url))
    ##         if (L == 1059) {
    ##             return(T)
    ##         }
    ##         if (L == 798) {
    ##             return(F)
    ##         }
    ##         if (!(L %in% c(798, 1059))) {
    ##             return(NA)
    ##         }
    ##     }
    ##     if (type == "live") {
    ##         Url <- "http://www.chess.com/livechess/game?id="
    ##         Url <- paste(Url, id, sep = "")
    ##         L <- length(readLines(Url))
    ##         if (L == 1008) {
    ##             return(T)
    ##         }
    ##         if (L == 798) {
    ##             return(F)
    ##         }
    ##         if (!(L %in% c(798, 1008))) {
    ##             return(NA)
    ##         }
    ##     }
    ## }
    ## 
    ## [[37]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[38]]
    ## [[38]]$value
    ## function (pgnFile) 
    ## {
    ##     pgnLines <- readLines(pgnFile)
    ##     notationLines <- (grep("\\d\\.[KQRNB]?[abcdefgh12345678]?x?[abcdefgh][1234567]", 
    ##         pgnLines))
    ##     range <- data.frame(start = grep("\\[Event", pgnLines), stringsAsFactors = F)
    ##     range <- cbind(range, stop = c(range[-1, "start"] - 1, length(pgnLines)), 
    ##         stringsAsFactors = F)
    ##     for (n in 1:nrow(range)) {
    ##         m <- seq(range[n, "start"], range[n, "stop"])
    ##         meta <- pgnLines[m][grep("\\[", pgnLines[m])]
    ##         pgn <- paste(pgnLines[m][!pgnLines[m] %in% meta], collapse = " ")
    ##         pgn <- gsub(" {2,}", " ", pgn)
    ##         meta <- unlist(strsplit(meta, "\""))
    ##         gsub("\\[| $", "", meta[grep("^\\[", meta)])
    ##         DF <- data.frame(matrix(meta[-grep("\\[|\\]", meta)], 
    ##             nrow = 1), stringsAsFactors = F)
    ##         colnames(DF) <- gsub("\\[| $", "", meta[grep("^\\[", 
    ##             meta)])
    ##         DF <- cbind(DF, pgn = pgn, stringsAsFactors = F)
    ##         if (n == 1) {
    ##             metaChess <- DF
    ##         }
    ##         else {
    ##             DF <- DF[, colnames(DF) %in% colnames(metaChess)]
    ##             metaChess <- rbind(metaChess, DF)
    ##         }
    ##     }
    ##     metaChess
    ## }
    ## 
    ## [[38]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[39]]
    ## [[39]]$value
    ## function (tidyDF = ALL, n, string = "onlinegame", lengthLimit = 2) 
    ## {
    ##     pgn <- as.vector(tidyDF[n, "pgn"])
    ##     pgn <- strsplit(pgn, " ")[[1]]
    ##     pgn <- pgn[grepl("[[:alpha:]]", pgn)]
    ##     if (length(pgn) < lengthLimit) {
    ##         return(cbind(gameID = n, pgn = NA))
    ##     }
    ##     else {
    ##         blck <- paste("...", pgn[seq(2, length(pgn), 2)], sep = "")
    ##         blck <- paste(seq(1, length(blck)), blck, sep = "")
    ##         pgn[seq(2, length(pgn), 2)] <- blck
    ##         return(cbind(gameID = paste(string, n, sep = ""), pgn))
    ##     }
    ## }
    ## 
    ## [[39]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[40]]
    ## [[40]]$value
    ## function (tidyDF = ALL, n, string = "beta", lengthLimit = 2) 
    ## {
    ##     pgn <- as.vector(tidyDF[n, "pgn"])
    ##     pgn <- strsplit(pgn, " ")[[1]]
    ##     pgn <- pgn[grepl("[[:alpha:]]", pgn)]
    ##     if (length(pgn) < lengthLimit) {
    ##         return(cbind(gameID = n, pgn = NA))
    ##     }
    ##     else {
    ##         blck <- paste("...", pgn[seq(2, length(pgn), 2)], sep = "")
    ##         blck <- paste(seq(1, length(blck)), blck, sep = "")
    ##         pgn[seq(2, length(pgn), 2)] <- blck
    ##         return(cbind(gameID = paste(string, n, sep = ""), pgn))
    ##     }
    ## }
    ## 
    ## [[40]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[41]]
    ## [[41]]$value
    ## function (game_pgn) 
    ## {
    ##     enPassant <- grepl("phantom", position[game_pgn, ], ignore.case = T)
    ##     enPassant <- colnames(position)[enPassant]
    ##     if (length(enPassant) == 0) {
    ##         enPassant <- "-"
    ##     }
    ##     ORD <- rep(letters[1:8], 8)
    ##     for (n in 1:8) {
    ##         m <- 8 * n
    ##         ORD[(m - 7):m] <- paste(ORD[m:(m - 7)], n, sep = "")
    ##     }
    ##     ORD <- rev(ORD)
    ##     string <- as.vector(as.matrix(position[game_pgn, ORD]))
    ##     string[is.na(string)] <- "1"
    ##     string[string == ""] <- "1"
    ##     string[grepl(chesspatterns$white, string)] <- toupper(string[grepl(chesspatterns$white, 
    ##         string)])
    ##     string[grepl(chesspatterns$black, string)] <- tolower(string[grepl(chesspatterns$black, 
    ##         string)])
    ##     string <- gsub("WHITE |black ", "", string)
    ##     string[grepl(chesspatterns$knight, string)] <- sub("[Kk]", 
    ##         "", string[grepl(chesspatterns$knight, string)])
    ##     string <- unlist(lapply(string, substr, start = 1, stop = 1))
    ##     string <- c(string[c(1:8)], "/", string[c(9:16)], "/", string[c(17:24)], 
    ##         "/", string[c(25:32)], "/", string[c(33:40)], "/", string[c(41:48)], 
    ##         "/", string[c(49:56)], "/", string[c(57:64)])
    ##     n <- 1
    ##     while (n <= length(string)) {
    ##         if (grepl("\\d", string[n])) {
    ##             while (grepl("\\d", string[n + 1])) {
    ##                 string[n] <- as.character(sum(as.numeric(string[c(n, 
    ##                   n + 1)])))
    ##                 string <- string[-(n + 1)]
    ##             }
    ##         }
    ##         n <- n + 1
    ##     }
    ##     string <- paste(string, collapse = "")
    ##     if (grepl(chesspatterns$black, row.names(position[game_pgn, 
    ##         ]))) {
    ##         string <- paste(string, "w")
    ##     }
    ##     else {
    ##         string <- paste(string, "b")
    ##     }
    ##     game <- row.names(position[game_pgn, ])
    ##     game <- strsplit(game, "_")[[1]][1]
    ##     game <- paste(game, "_", sep = "")
    ##     game <- grep(game, row.names(position))
    ##     game <- game[game <= which(row.names(position) == row.names(position[game_pgn, 
    ##         ]))]
    ##     tests <- rep(NA, 4)
    ##     names(tests) <- c("K", "Q", "k", "q")
    ##     temp <- unique(as.vector(as.matrix(position[game, c("a1", 
    ##         "e1")])))
    ##     tests["K"] <- length(temp) == 2
    ##     temp <- unique(as.vector(as.matrix(position[game, c("h1", 
    ##         "e1")])))
    ##     tests["Q"] <- length(temp) == 2
    ##     temp <- unique(as.vector(as.matrix(position[game, c("a8", 
    ##         "e8")])))
    ##     tests["k"] <- length(temp) == 2
    ##     temp <- unique(as.vector(as.matrix(position[game, c("h8", 
    ##         "e8")])))
    ##     tests["q"] <- length(temp) == 2
    ##     tests <- tests[tests]
    ##     if (length(tests) == 0) {
    ##         tests <- "-"
    ##     }
    ##     else {
    ##         tests <- paste(names(tests), collapse = "")
    ##     }
    ##     string <- paste(string, tests)
    ##     string <- paste(string, enPassant)
    ##     halfClock <- row.names(position)[game]
    ##     halfClock <- rev(!grepl("[KQRNB]", halfClock) | grepl("x", 
    ##         halfClock))
    ##     halfClock <- min(which(halfClock))
    ##     string <- paste(string, halfClock)
    ##     fullMove <- unlist(strsplit(row.names(position[game_pgn, 
    ##         ]), "\\.|_"))
    ##     fullMove <- fullMove[min(grep("^\\d+$", fullMove))]
    ##     string <- paste(string, fullMove)
    ##     string
    ## }
    ## 
    ## [[41]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[42]]
    ## [[42]]$value
    ## function (DF1, DF2) 
    ## {
    ##     for (n in 1:ncol(DF1)) {
    ##         if (!colnames(DF1)[n] %in% colnames(DF2)) {
    ##             DF2[, colnames(DF1)[n]] <- ""
    ##         }
    ##     }
    ##     for (n in 1:ncol(DF2)) {
    ##         if (!colnames(DF2)[n] %in% colnames(DF1)) {
    ##             DF1[, colnames(DF2)[n]] <- ""
    ##         }
    ##     }
    ##     return(rbind(DF1, DF2, stringsAsFactors = F))
    ## }
    ## 
    ## [[42]]$visible
    ## [1] FALSE

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
