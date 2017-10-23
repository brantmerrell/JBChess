

mobilePlot <- function(mobilePattern = "total", 
                       DF = mobility,
                       Legend = T,
                       legendX = "topright"){
  mobileCol <- grep(mobilePattern,colnames(DF))
  plot(as.ts(DF[,mobileCol[1]]), 
       col = 'red', 
       ylim = c(0, max(c(DF[,mobileCol[1]], DF[,mobileCol[2]]))),
       ylab = "mobility",
       xlab = paste(unique(gsub("white|black","",colnames(DF)[mobileCol])),collapse = ","))
  lines(as.ts(DF[,mobileCol[2]]), col = 'blue')
  if(Legend){
    legend(legendX,legend = c("white","black"),fill = c("red","blue"))
  }
}

tidyToPgn <- function(tidyDF=ALL,n=1,string="xmpl", lengthLimit = 2){
  pgn <- as.vector(tidyDF[n,"pgn"])
  pgn <- strsplit(pgn," ")[[1]]
  pgn <- pgn[grepl('[[:alpha:]]',pgn)]
  if(length(pgn)<lengthLimit){
    return(cbind(gameID=n,pgn=NA))
  }else{
    blck <- paste("...",pgn[seq(2,length(pgn),2)],sep="")
    blck <- paste(seq(1,length(blck)),blck,sep="")
    pgn[seq(2,length(pgn),2)] <- blck
    return(cbind(gameID=paste(string,n,sep=""),pgn))
  }
    # ifelse(n==1,pgnDF <- DF,pgnDF <- rbind(pgnDF,DF))
  # }
}
pgnFile <- "www.chess.com/livechess/analysis?id=1022018652"
rawToTidy <- function(pgnFile){
  if(grepl("chess.com",pgnFile)){
    if(!grepl("@",pgnFile)){
      pgnFile <- paste("https://thinkboolean:witstorm666",
                       gsub("https://","",pgnFile),
                       sep="@")
    }
    if(!grepl("livechess",pgnFile)){
      con = url(pgnFile)
      htmlCode = readLines(con)
      close(con)
      pgn <- unlist(strsplit(htmlCode[grep("\\d\\.[[:lower:]]\\d",htmlCode)],"Board\\(\\'|\\'\\)"))
      pgn <- pgn[grepl("\\d\\.[[:lower:]]\\d",pgn)]
      pgn <- strsplit(pgn,"\\+(?!\\+)",perl = T)[[1]]
      Title <- htmlCode[grep("<title>",htmlCode,ignore.case = T)]
      Title <- gsub("</?title>| {2}","",Title)
      metaChess <- data.frame(Title,pgn = paste(pgn,collapse = " "))
    }else{
      message("cannot currently extract live games")
    }
  }else{
    pgnLines <- readLines(pgnFile)
    notationLines <- 
      (grep("\\d\\.[KQRNB]?[abcdefgh12345678]?x?[abcdefgh][1234567]",pgnLines))
    range <- data.frame(start=grep("\\[Event",pgnLines))
    range <- cbind(range,"stop"=c(range[-1,'start']-1,length(pgnLines)))
    for(n in 1:nrow(range)){
      m <- seq(range[n,'start'],range[n,'stop'])
      meta <- pgnLines[m][grep("\\[",pgnLines[m])]
      pgn <- paste(pgnLines[m][!pgnLines[m] %in% meta], collapse = " ")
      pgn <- gsub(" {2,}"," ",pgn)
      meta <- unlist(strsplit(meta,"\""))
      gsub("\\[| $","",meta[grep("^\\[",meta)])
      DF <- data.frame(matrix(meta[-grep("\\[|\\]",meta)],nrow=1))
      colnames(DF) <- gsub("\\[| $","",meta[grep("^\\[",meta)])
      DF <- cbind(DF,pgn=pgn)
      if(n==1){metaChess<-DF}else{
        DF <- DF[,colnames(DF) %in% colnames(metaChess)]
        metaChess <- rbind(metaChess,DF)
      }
    }
  }
  metaChess
}

# betweens() generates squares and occupants between two squares for position game_pgn
#   betweens <- function(square1,square2,game_pgn){...data.frame(squares,occupants)}
betweens <- function(square1,square2,game_pgn){
  Col1 <- which(letters==strsplit(square1,"")[[1]][1]) # convert square's column letter to a number
  Row1 <- as.numeric(strsplit(square1,"")[[1]][2]) # extract square's row number
  Col2 <- which(letters==strsplit(square2,"")[[1]][1]) # convert square2 column letter to a number
  Row2 <- as.numeric(strsplit(square2,"")[[1]][2]) # extract square2 row number
  if(length(Col1:Col2)!=length(Row1:Row2) & # check whether squares do NOT share a diagonal
       !(0 %in% c(Col1-Col2,Row1-Row2))){  # check whether squares do NOT share a line
    # if none of these, something is wrong. Send message:
    stop("these two squares are not in the same horizontal, vertical, or diagnal")
  }
  btwns <- paste(letters[Col1:Col2],Row1:Row2,sep="") #list squares between the squares
  return(data.frame(squares=btwns,
                    occupants=as.character(position[game_pgn,btwns]))) #retrieve occupants
}

squareColor <- function(square="a1",game_pgn=2){
  patternVec <- c(blackPattern,whitePattern)
  patternTest <- unlist(lapply(patternVec, grepl, x = position[game_pgn,square]))
  ifelse(sum(patternTest)==0, return(NA), 
         return(patternVec[patternTest]))
}


pieceCount <- function(game_pgn=nrow(position)){
  
  # identify which part of the board is occupied
  Col <- apply(position[game_pgn,],2,is.na)
  
  # identify the pieces on the occupied portion of the
  
  if(!sum(Col)==1){
    square <- colnames(position)[Col]
    piece <- as.character(as.matrix(position[game_pgn,Col]))
    DF<-data.frame(square=colnames(position[game_pgn,Col]),
                   piece=as.character(as.matrix(position[game_pgn,Col])))
  }else{
    DF<-data.frame(square=colnames(position[game_pgn,Col]),
                   piece=piece)
  }
  DF
}

board <- function(game_pgn=nrow(position),command = "View,return"){
  # the board should be neither a 64x2 data frame nor a list of squares, but an 8x8 data frame:
  DF <- data.frame(matrix(position[game_pgn,],nrow=8,ncol=8),row.names=8:1)
  
  # its column names should be letters:
  colnames(DF) <- letters[1:8]
  
  DF <- apply(DF,c(1,2),gsub, pattern=" ",replacement=" \n ")
  if(grepl("View",command)){View(DF)}
  if(grepl("return",command)){return(as.matrix(DF))}
}

pathPrior <- function(piece,square){
  Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  
  # if the piece is a pawn:
  if(grepl(pawnPattern,piece)){
    
    # if the pawn's color has not yet been defined, define it as nothing. Why?
    if(!exists("color")){color <- ""}
    
    # if the pawn is black, it moves down the board:
    if(grepl(blackPattern,piece)){direction <- -1} 
    
    # if the pawn is white, it moves up the board:
    if(grepl(whitePattern,piece)){direction <- 1} 
    
    # create function generating attack columns:
    PRange <- function(center){ 
      
      # attack columns are the current column plus & minus 1...
      P <- c((center-1),(center+1)) 
      
      # ... minus any columns outside board boundaries
      P[0<P & P<=8] 
    }
    
    # generate pawn attack squares:
    AttSq <- paste(letters[PRange(Col)],Row+direction,sep="")
    
    # determine whether pawn has option to move two spaces:
    ifelse((direction==-1 & Row==7)|(direction==1 & Row==2),distance <- 1:2,distance <- 1)
    
    # generate squares to which the pawn can move without attacking:
    Path <- paste(letters[Col],Row+(distance*direction),sep="") 
    
    # return all squares to which the pawn can move:
    Path <- c(Path,AttSq)
  }
  if(grepl(bishopPattern,piece)){ # check whether the piece is a bishop
    B <- function(x){ # define a function that takes as input any row, 
      V <- c(x+Col-Row,Col+Row-x) # already knows bishop location, 
      paste(letters[x],V[0<V & V<=8],sep="") 
      # and returns squares on that row where the bishop can move
    }
    Path <- unique(unlist(sapply(1:8,B))) # input all rows on the board
  }
  if(grepl(knightPattern,piece)){ # check whether the piece is a knight
    NRange <- function(center,dist){ # create 1-dim function for plus & minus board distance
      dist <- unique(c(dist,dist*-1)) # distance = plus and minus displacement
      N <- expand.grid(center,dist) # combinations of center and displacements
      N <- apply(N,1,sum) # add center and displacements for 1-d location
      N[0<N & N<=8] # return only what exists within board boundaries
    }
    Path <- rbind(expand.grid(letters[NRange(Col,1)],NRange(Row,2)), # generate 1xCol and 2xRow
                expand.grid(letters[NRange(Col,2)],NRange(Row,1))) # generate 2xCol and 1xRow
    Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
  }
  if(grepl(rookPattern,piece)){ # check whether the piece is a rook
    Path <- unique(c(paste(letters[Col],1:8,sep=""), # generate squares along column
                   paste(letters[1:8],Row,sep=""))) # generate squares along row
  }
  if(grepl(queenPattern,piece)){ # check whether the piece is a Queen
    # replicate bishop function:
    B <- function(x){ 
      V <- c(x+Col-Row,Col+Row-x) 
      paste(letters[x],V[0<V & V<=8],sep="")
    }
    Path <- c(unique(unlist(sapply(1:8,B))), # generate diagonals
            unique(c(paste(letters[Col],1:8,sep=""), # generate squares along column
                     paste(letters[1:8],Row,sep="")))) # generate squares along row
  }
  if(grepl(kingPattern,piece)){ # check whether the piece is a King
    KRange <- function(center){ # create 1-dim function for plus to minus board distance (1) 
      K <- (center-1):(center+1) # broaden center in two directions
      K[0<K & K<=8] # return only results within board boundaries
    }
    Path <- expand.grid(letters[KRange(Col)], # generate all combinations of King's column range
                      KRange(Row)) # generate all combinations of King's row range
    Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
  }
  Path[Path!=square] # a piece can't move to the square on which it already resides!
}

# _Post.() functions generate squares to which a piece could 
# move on the board at a given row name of the position frame.
lookBack <- function(game_pgn=nrow(position),n=1){
  if(grepl("^\\d+$",game_pgn)){
    return(row.names(position)[as.numeric(game_pgn)-1])
  }
  if(class(game_pgn)=="character"){
    return(row.names(position)[which(row.names(position)==game_pgn)-1])
  }
}

pawnLeap <- function(game_pgn=nrow(position)){
  square <- strsplit(row.names(position)[game_pgn],"\\.|_")[[1]]
  square <- square[length(square)]
  square <- gsub("[[:punct:]]","",square)
  column <- strsplit(square,"")[[1]]
  row <- column[grepl("\\d",column)]
  column <- column[grepl("[abcdefgh]",column)]
  
  if(row==5){fromRow <- 7;midRow <- 6}
  if(row==4){fromRow <- 2;midRow <- 3}
  fromSquare <- paste(column,fromRow,sep=""); midSquare <- paste(column,midRow,sep="")
  if(!row %in% c(4,5)){return(F)}else{
    isPawn <- grepl(pawnPattern,row.names(position)[game_pgn])
    didntCapture <- !grepl("x",row.names(position)[game_pgn])
    leapTo <- grepl("[abcdefg][45]",row.names(position)[game_pgn])
    leapFrom <- position[game_pgn,square]==position[lookBack(game_pgn,1),fromSquare]
    midVacant <- is.na(position[lookBack(game_pgn,1),midSquare])
    return(isPawn & didntCapture & leapTo & leapFrom & midVacant)
  }
}

neighborPassant <- function(game_pgn=nrow(position)){
  square <- strsplit(row.names(position)[game_pgn],"\\.|_")[[1]]
  square <- square[length(square)]
  square <- gsub("[[:punct:]]","",square)
  square <- strsplit(square,"")[[1]]
  square <- paste(square[length(square)-(1:0)],collapse="")
  piece <- position[game_pgn,square]
  Column <- strsplit(square,"")[[1]]
  Row <- Column[grepl("\\d",Column)]
  Column <- Column[grepl("[abcdefgh]",Column)]
  
  neighborCol <- letters[which(letters==Column)+c(-1,1)]
  neighborSquare <- paste(neighborCol,Row,sep="")
  neighborPiece <- position[game_pgn,neighborSquare]
  
  arePawns <- grepl("pawn",piece) & grepl("pawn",neighborPiece)
  diffColors <- (grepl(blackPattern,piece) & grepl(whitePattern,neighborPiece)) |
    grepl(whitePattern,piece) & grepl(blackPattern, neighborPiece)
  passantRows <- grepl("[45]",square) & grepl("[45]",neighborSquare)
  ifelse(F %in% c(arePawns, diffColors, passantRows),return(F),return(T))
}

pawnPost. <- function(square,game_pgn=nrow(position)){
  
  # specify direction for black pawns
  if(grepl(blackPattern,position[game_pgn,square])){direction <- -1}
  
  # specify direction for white pawns
  if(grepl(whitePattern,position[game_pgn,square])){direction <- 1}
  
  # specify double direction for unspecified pawns
  if(!grepl(paste(blackPattern,whitePattern, sep="|"), position[game_pgn,square])){
    direction <- c(1,-1)
  }
  
  # identify the corresponding number to the column letter
  Col <- which(letters==strsplit(square,"")[[1]][1]) 
  
  # identify the column row
  Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  
  # identify the columns which the pawn can attack:
  AttRange <- c((Col-1),(Col+1))
  
  # remove any 'attack columns' that do not exist on the board:
  AttRange <- AttRange[0<AttRange & AttRange<=8]
  
  # use the pawn's direction to identify the row it can attack:
  AttSq <- paste(letters[AttRange],Row+direction,sep="")
  
  # deduce whether the pawn has moved based on its color and row:
  ifelse((direction==-1 & Row==7)|(direction==1 & Row==2),
         
         # if the pawn has moved, it has the option of moving two spaces:
         distance <- 1:2,
         
         # if the pawn has not moved, it can only move one space:
         distance <- 1)
  
  # obtain the pawn's non-attack destinations by adding its row to its distance and direction:
  normSq <- paste(letters[Col],Row+(distance*direction),sep="")
  
  # create a function to test whether an attackable piece is on a square:
  AttTest <- function(AttSquare,fromSquare=square){
    
    # if the attacking and defending pieces are different colors... 
    ifelse((grepl(blackPattern,position[game_pgn,fromSquare]) & # black attacking pience & 
              grepl(whitePattern,position[game_pgn,AttSquare])) | # white defending piece OR
             (grepl(whitePattern,position[game_pgn,fromSquare]) & # white attacking piece & 
                grepl(blackPattern,position[game_pgn,AttSquare])), # black defending piece
           # . . . return TRUE, otherwise FALSE
           T,F)
  }
  
  # create a function to test whether the pawn can move to a non-attacking square:
  normTest <- function(normSquare,fromSquare=square){
    
    # create path to find out whether pawn would be required hop over or capture any pieces:
    DF <- betweens(normSquare,fromSquare,game_pgn)
    
    # remove pawn's current location from generated path:
    DF <- DF[DF$squares!=fromSquare,]
    
    # test whether each space is clear:
    clear <- unlist(lapply(DF$occupants,is.na))
    
    # return logical on whether all spaces are clear:
    sum(clear)==nrow(DF)
  }
  
  # remove any attack squares that do not pass the pawn attack test:
  AttSq <- AttSq[unlist(lapply(AttSq,AttTest))]
  
  # remove any non-attack squares that do not pass the "clear path" test:
  normSq <- normSq[unlist(lapply(normSq,normTest))]
  
  # return all potential destinations, or if none exist, return NA
  ifelse(0<length(c(normSq,AttSq)),
         return(c(normSq,AttSq)),
         return(NA))
}

knightPost. <- function(square,game_pgn=nrow(position)){
  Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  NRange <- function(center,dist){ # create 1-dim function for plus & minus board distance
    dist <- unique(c(dist,dist*-1)) # distance = plus and minus displacement
    N <- expand.grid(center,dist) # combinations of center and displacements
    N <- apply(N,1,sum) # add center and displacements for 1-d location
    N[0<N & N<=8] # return only what exists within board boundaries
  }
  Path <- rbind(expand.grid(letters[NRange(Col,1)],NRange(Row,2)), # generate 1xCol and 2xRow
                expand.grid(letters[NRange(Col,2)],NRange(Row,1))) # generate 2xCol and 1xRow
  Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
  Test <- function(toSquare,fromSquare=square){
    testPattern <- ifelse(grepl(blackPattern,position[game_pgn,fromSquare]),
                          whitePattern,blackPattern)
    testPattern <- paste(testPattern,"phantom",sep="|")
    grepl(testPattern,position[game_pgn,toSquare]) |
      is.na(position[game_pgn,toSquare])
  }
  Path[unlist(lapply(Path,Test))]
}

bishopPost. <- function(square,game_pgn){
  
  # identify row and column:
  Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  
  # list the 'cartesian first quadrant' squares: 
  Lim <- min(length(Col:8),length(Row:8))
  Path <- paste(letters[Col:8][1:Lim],(Row:8)[1:Lim],sep="")
  
  # add the 'cartesian third quadrant' squares:
  Lim <- min(length(Col:1),length(Row:1))
  Path <- c(Path,paste(letters[Col:1][1:Lim],(Row:1)[1:Lim],sep=""))
  
  # add the 'cartesian fourth quadrant' squares:
  Lim <- min(length(Col:8),length(Row:1))
  Path <- c(Path,paste(letters[Col:8][1:Lim],(Row:1)[1:Lim],sep=""))
  
  # add the 'cartesian second quadrant' squares:
  Lim <- min(length(Col:1),length(Row:8))
  Path <- c(Path,paste(letters[Col:1][1:Lim],(Row:8)[1:Lim],sep=""))
  
  # because pieces cannot move to a square they occupy, remove occupied square from list:
  Path <- Path[Path!=square]
  
  # remove duplicate squares
  Path <- sort(unique(Path))
  
  # create a test indicating whether destination is compatible:
  Test <- function(toSquare,fromSquare=square){
    
    # isolate the color of moving piece
    fromColor <- squareColor(fromSquare,game_pgn)
    
    # store the color that the moving piece can capture
    if(!is.na(fromColor)){
      if(fromColor==whitePattern){colorPattern <- blackPattern}
      if(fromColor==blackPattern){colorPattern <- whitePattern}
      
      # if the moving piece has no listed color, allow to capture either color
    }else{colorPattern <- paste(whitePattern,blackPattern,sep="|")}
    
    # define the color of the destination square
    toColor <- squareColor(toSquare,game_pgn)
    
    # if the destination square is a phantom pawn, set its color as NA
    if(grepl("phantom",position[game_pgn,toSquare])){toColor <- NA}
    
    # map out pieces and squares between starting square and destination square
    DF <- betweens(toSquare,fromSquare,game_pgn)
    
    # test whether 'between squares' are vacant by taking the sum of vacant squares,
    T1 <- (sum(is.na(DF$occupants[
      
      # (excluding start and end squares)
      -c(1,nrow(DF))]) |
        
        # adding phantom squares as vacant,
        grepl("phantom",DF$occupants[-c(1,nrow(DF))]))
      
      # and testing whether this sum equals between squares (exclusive of start & dest)
      ==nrow(DF)-2)
    
    # test whether destination square is a piece that can be captured by moving piece
    T2a <- grepl(colorPattern,DF[DF$squares==toSquare,"occupants"])
    
    # test whether destination square is vacant or phantom
    T2b <- is.na(DF[DF$squares==toSquare,"occupants"]) | 
      grepl("phantom",DF[DF$squares==toSquare,"occupants"])
    
    # summarize whether moving piece can travel to and arrive on destination 
    T1 & (T2a | T2b)
  }
  Path[unlist(lapply(Path,Test))]
}

rookPost. <- function(square,game_pgn){
  
  # isolate column and rows of square
  Col <- which(letters==strsplit(square,"")[[1]][1]) 
  Row <- as.numeric(strsplit(square,"")[[1]][2])
  
  # generate rook path for empty board
  Path <- unique(c(paste(letters[Col],1:8,sep=""),paste(letters[1:8],Row,sep="")))
  
  # remove rook's current square from path
  Path <- sort(Path[Path!=square])
  
  # create test to eliminate faulty squares from path
  Test <- function(toSquare,fromSquare=square){
    
    # determine the color of the moving piece
    fromColor <- squareColor(fromSquare,game_pgn)
    
    # store the color that can be captured by moving piece
    if(!is.na(fromColor)){
      if(fromColor==whitePattern){colorPattern <- blackPattern}
      if(fromColor==blackPattern){colorPattern <- whitePattern}
      
    # if moving piece color is unspecified, default to white
    }else{colorPattern <- blackPattern}
    
    # define the color of the destination square
    toColor <- squareColor(toSquare,game_pgn)
    
    # if the destination square is a phantom pawn, set its color as NA
    if(grepl("phantom",position[game_pgn,toSquare])){toColor <- NA}
    
    # map out squares and pieces between start and destination squares
    DF <- betweens(toSquare,fromSquare,game_pgn)
    
    # test whether squares between start and destination are clear:
    T1 <- (sum(is.na(DF$occupants[
      
      # (exclude start and destination squares)
      -c(1,nrow(DF))]) | 
        
        # add squares with phantom pawns as vacant
        grepl("phantom",DF$occupants[-c(1,nrow(DF))]))
           
           # make sure the number of clear squares equals the exclusive 'between distance'
           ==nrow(DF)-2)
    
    # test whether destination square is a piece that can be captured by moving piece
    T2a <- grepl(colorPattern,DF[DF$squares==toSquare,"occupants"])
    
    # test whether destination square is vacant or phantom
    T2b <- is.na(DF[DF$squares==toSquare,"occupants"]) | 
      grepl("phantom",DF[DF$squares==toSquare,"occupants"])
    
    # summarize whether moving piece can travel to and arrive on destination 
    T1 & (T2a | T2b)
  }
  Path[unlist(lapply(Path,Test))]
}

empty <- function(gameName = "000"){
  # to create board, first list each combination of columns a:h & rows 1:8:
  board <- expand.grid(columns=letters[1:8],rows=1:8)
  
  # arrange columns and rows such that they will read left-to-right and bottom-to-top:
  board <- board[order((board$columns), 
                     rev(board$rows)),] 
  # - this can feel counter-intuitive in <r>, as data frames typically begin at the top-left
  
  # convert data frame to vector:
  board <- paste(board[,1],board[,2],sep="")
  
  # since this "board" is now linear, it's essentially a list of squares:
  squares <- board
  
  # Every position of every piece must be available for every portion of every game.
  # It could be awkward storing a separate data frame per move per game, 
  # so it's useful to have a 64-column data frame with an indefinite number of rows,
  # particularly if rows can be accessed by a game_pgn combination:
  
  position <- data.frame(matrix(NA,nrow=1,ncol=64))
  row.names(position) <- paste(gameName,"empty", sep = "_")
  # - "empty" refers to an empty chessboard, "zero" is a set-up chessboard with no moves made
  
  # the column names for the position frame should be the list of squares:
  colnames(position) <- squares
  position
}

setup <- function(gameName = "000", includeEmpty = TRUE){
  position <- empty(gameName)
  position <- rbind(empty(),empty())
  
  row.names(position) <- paste(gameName, c("empty","zero"),sep = "_")
  # Since position has a column for each square, we set up the chessboard column-by column.
  # First identify the color of the piece that will belong on each square:
  for(Col in 1:ncol(position)){
    class(position[,Col]) <- "character" # this makes the process smoother for <r> reasons 
    
    # pieces on rows 1 and 2 are white:
    if(grepl("(1|2)$",colnames(position)[Col])){ 
      position[2,Col] <- "white"
    }
    
    # pieces on rows 7 and 8 are black:
    if(grepl("(7|8)$",colnames(position)[Col])){
      position[2,Col] <- "black"
    }
  }
  
  # now that color is finished, add piece:
  for(Col in 1:ncol(position)){
    
    # pieces on rows 2 and 7 are pawns:
    if(grepl("(7|2)$",colnames(position)[Col])){
      position[2,Col] <- paste(position[2,Col],"pawn")
    }
    
    # pieces on columns a & h and rows 1 & 8 are rooks:
    if(grepl("(a|h)(1|8)",colnames(position)[Col])){
      position[2,Col] <- paste(position[2,Col],"Rook")
    }
    
    # pieces on columns b & g and rows 1 & 8 are knights:
    if(grepl("(b|g)(1|8)",colnames(position)[Col])){
      position[2,Col] <- paste(position[2,Col],"Knight")
    }
    
    # pieces on columns c & f and rows 1 & 8 are bishops:
    if(grepl("(c|f)(1|8)",colnames(position)[Col])){
      position[2,Col] <- paste(position[2,Col],"Bishop")
    }
    
    # Queens are on column d:
    if(grepl("d(1|8)",colnames(position)[Col])){
      position[2,Col] <- paste(position[2,Col],"Queen")
    }
    
    # Kings are on column e:
    if(grepl("e(1|8)",colnames(position)[Col])){
      position[2,Col] <- paste(position[2,Col],"King")
    }
  }
  if(includeEmpty==FALSE){position <- position[nrow(position),]}
  position
}

read.position <- function(filePath){
  data.frame(read.csv(filePath),row.names="X")
}

pathPost. <- function(square,game_pgn){
  piece <- as.vector(position[game_pgn,square])
  if(grepl(pawnPattern,piece)){return(pawnPost.(square,game_pgn))}
  if(grepl(knightPattern,piece)){return(paste("",knightPost.(square,game_pgn),sep=""))}
  if(grepl(bishopPattern,piece)){return(paste("",bishopPost.(square,game_pgn),sep=""))}
  if(grepl(rookPattern,piece)){return(paste("",rookPost.(square,game_pgn),sep=""))}
  if(grepl(queenPattern,piece)){return(paste("",queenPost.(square,game_pgn),sep=""))}
  if(grepl(kingPattern,piece)){return(paste("",kingPost.(square,game_pgn),sep=""))}
}

newPosition <- function(new_pgn, startPosition=nrow(position)){
  
  # determine whether the move is a 'castle', in which king and rook both move:
  castle <- grepl(castlePattern,new_pgn)
  
  # determine whether the move is a pawn promotion:
  promotion <- grepl("=",new_pgn)
  
  # determine whether a black or a white piece is moving:
  color <- ifelse(grepl(blackPattern,new_pgn),"black","white")
  
  # determine which move of the game is being assessed:
  move <- strsplit(new_pgn,"\\.|_")[[1]]
  move <- as.numeric(move[grep("^\\d+$",move)])
  
  # if this move is not a 'castle',
  if(!castle & !promotion){
    
    # obtain the full name of the moving piece, including its color:
    piece <- paste(color,pgnpiece(new_pgn))
    
    # extract the name of the square to which the piece is moving:
    endSquare <- gsub("[[:punct:]]$","",new_pgn)
    endSquare <- strsplit(endSquare,"")[[1]]
    endSquare <- paste(endSquare[(length(endSquare)-1):length(endSquare)],collapse="")
    
    # to determine the vacant square, 
    # first list pieces that match the type and color of the moving piece:
    workFrame <- pieceCount(startPosition)
    workFrame <- workFrame[grepl(color,workFrame$piece),]
    workFrame <- workFrame[grepl(tolower(piece),tolower(as.vector(workFrame$piece))),]
    
    # list the potential destinations of all pieces listed above,
    workList <- lapply(as.character(workFrame$square), pathPost., game_pgn=startPosition)
    
    # name the list of potential destinations based on potential starting points: 
    names(workList) <- as.character(workFrame$square)
    
    # write a test specifying whether a starting point is valid for the destination:
    TEST <- function(n){endSquare %in% workList[[n]]}
    
    # test each of the starting points:
    startSquare <- which(unlist(lapply(1:length(workList),TEST)))
    
    # if more than one starting point is still possible, 
    # the input pgn will contain more specific information:
    if(1<length(startSquare)){
      
      # the pattern to differentiate will be separate from the endsquare,
      fromPattern <- gsub(endSquare,"",new_pgn)
      
      # it will be located to the right of any pgn dots,
      fromPattern <- tail(strsplit(fromPattern,"\\.")[[1]],1)
      
      # and it will not be uppercase letters, punctuation, or the capture symbol.
      fromPattern <- gsub("[[:upper:]]|[[:punct:]]|x","",fromPattern)
      
      # so now eliminate startSquare numbers that do not fit fromPattern: 
      startSquare <- startSquare[grepl(fromPattern,
                                       names(workList)[startSquare])]
    }
    
    # the startsquare is in digit form and must convert to square names:
    startSquare <- names(workList)[[startSquare]]
    
    # define the new position as the old position, modify later:
    newPosition <- position[startPosition,]
    
    # define the rowname of the new position as the new input pgn:
    row.names(newPosition) <- new_pgn
    
    # remove the moving piece from its starting point:
    newPosition[,startSquare] <- NA
    
    # add the moving point to its destination point:
    newPosition[,endSquare] <- piece
    
    # if moving piece is pawn and destination point is phantom, remove concrete pawn:
    if(grepl(pawnPattern,piece) & grepl("phantom",position[startPosition,endSquare])){
      Col <- which(letters==strsplit(endSquare,"")[[1]][1]) # identify the column
      if(grepl("^.6$",endSquare)){
        concretePawn <- paste(letters[Col],"5",sep="")
      }
      if(grepl("^.3$",endSquare)){
        concretePawn <- paste(letters[Col],"4",sep="")
      }
      newPosition[,concretePawn] <- NA
    }
    
    # detect and remove any phantom pawns, because they only exist for one move:
    phanP<-as.vector(apply(newPosition,2,grepl,pattern="phantom"))
    newPosition[,phanP] <- NA
    
    # if piece was a pawn and it moved two spaces, create 'phantom pawn' for potential en passant
    if(grepl(pawnPattern,piece) & grepl("2|7",startSquare) & grepl("4|5",endSquare)){
      Col <- which(letters==strsplit(startSquare,"")[[1]][1]) # identify the column
      if(color=="black"){phantomSquare<-paste(letters[Col],"6",sep="")}
      if(color=="white"){phantomSquare<-paste(letters[Col],"3",sep="")}
      newPosition[,phantomSquare]<-paste("phantom",color,"pawn")
    }
  }
  
  # if the move is a castle,
  if(castle){
    
    # the moving pieces consist of a king and a rook: 
    piece <- paste(kingPattern,rookPattern,sep="|")
    
    # use CSTL() to define the destination squares of the king and rook:
    endSquare <- unlist(CSTL(new_pgn)[c("king","rook")])
    
    # use CSTL() to define the starting squares of the king and rook:
    startSquare <- unlist(CSTL(new_pgn)[c("kingVacant","rookVacant")])
    
    # generate a new position from the old position:
    newPosition <- position[startPosition,]
    
    # name the new position as the new input pgn:
    row.names(newPosition) <- new_pgn
    
    # remove the King and Rook from their squares:
    newPosition[,startSquare] <- NA
    
    # place the King and Rook on their destination squares:
    newPosition[,endSquare] <- paste(color,names(endSquare))
  }
  
  # if the move is a promotion:
  if(promotion){
    
    # obtain the full name of the resulting piece, including its color:
    piece <- paste(color,pgnpiece(new_pgn))
    
    # extract the name of the square to which the piece is moving:
    endSquare <- strsplit(new_pgn,"[[:punct:]]")[[1]]
    if(grepl("_",new_pgn)){
      endSquare <- endSquare[-1]
    }
    endSquare <- endSquare[grepl("[[:lower:]]\\d$",endSquare)]
    endSquare <- paste(tail(strsplit(endSquare,"")[[1]],2),collapse="")
    
    # to determine the vacant square, 
    # first list pawns that match working color:
    workFrame <- pieceCount(startPosition)
    workFrame <- workFrame[grepl(color,workFrame$piece),]
    workFrame <- workFrame[grepl("pawn",tolower(workFrame$piece)),]
    
    # list the potential destinations of the pawns:
    workList <- lapply(as.character(workFrame$square), pathPost., game_pgn=startPosition)
    
    # name the list of potential destinations based on pawn locations: 
    names(workList) <- as.character(workFrame$square)
    
    # write a test specifying whether a starting point is valid for the destination:
    TEST <- function(n){endSquare %in% workList[[n]]}
    
    # test each of the starting points:
    startSquare <- which(unlist(lapply(1:length(workList),TEST)))
    
    # if more than one starting point is still possible, 
    # the input pgn will contain more specific information:
    if(1<length(startSquare)){
      
      # the pattern to differentiate will be to the left of the equal:
      fromPattern <- strsplit(new_pgn,"=")[[1]][1]
      
      # it will not be the endsquare,
      fromPattern <- gsub(endSquare,"",new_pgn)
      
      # it will be located to the right of any pgn dots,
      fromPattern <- tail(strsplit(fromPattern,"\\.")[[1]],1)
      
      # and it will not be uppercase letters, punctuation, or the capture symbol.
      fromPattern <- gsub("[[:upper:]]|[[:punct:]]|x","",fromPattern)
      
      # so now eliminate startSquare numbers that do not fit fromPattern: 
      startSquare <- startSquare[grepl(fromPattern,
                                       names(workList)[startSquare])]
    }
    
    # the startsquare is in digit form and must convert to square names:
    startSquare <- names(workList)[[startSquare]]
    
    # define the new position as the old position, modify later:
    newPosition <- position[startPosition,]
    
    # define the rowname of the new position as the new input pgn:
    row.names(newPosition) <- new_pgn
    
    # remove the moving piece from its starting point:
    newPosition[,startSquare] <- NA
    
    # add the moving point to its destination point:
    newPosition[,endSquare] <- piece
  }
  
  # return the new position:
  newPosition
}


pgnpiece <- function(pgn){
  
  piece <- names(piecePatterns)[unlist(lapply(X = ))]
  
  test <- function(L){grepl(L,pgn)}
  
  P <- c("K","Q","R","N","B")
  Piece=c("King","Queen","Rook","Knight","Bishop")
  df <- data.frame(P=P, Piece=Piece,assess=unlist(lapply(P,test)))
  if(sum(df$assess)==0){
    return("pawn")
  }
  if(sum(df$assess)==1){
    return(as.character(subset(df$Piece,df$assess==TRUE)))
  }
  if(1<sum(df$assess)){
    stop("Input contains extra piece")
  }
}

mobilityMap <- function(game_pgn){
  DF <- cbind(pieceCount(game_pgn),options="",count=NA, pgn = NA)
  class(DF$options) <- "character"
  for(n in 1:nrow(DF)){
    # n <- n-1
    options1 <- pathPost.(as.character(DF$square[n]),game_pgn)
    DF[n,"options"] <- paste(options1,collapse=" ")
    DF[n,"count"] <- ifelse(grepl(".\\d",DF[n,"options"]),length(options1),0)
    vacant <- is.na(position[game_pgn,options1]) | grepl("phantom",position[game_pgn,options1])
    DF[n,]
  }
  DF
  #   workList <- lapply(as.character(DF$square),pathPost.,game_pgn=game_pgn)
  #   for(n in 1:length(workList)){workList[[n]] <- paste(workList[[n]],collapse=" ")}
  #   names(workList) <- as.character(DF$piece)
  #   head(workList)
}

mobilityPost. <- function(game_pgn=2,piecePattern=""){
  DF <- mobilityMap(game_pgn)
  DF <- DF[grepl(piecePattern,DF$piece),]
  if(class(game_pgn) %in% c("numeric", "integer")){game_pgn <- row.names(position)[game_pgn]}
  data.frame(white=sum(DF[grepl(whitePattern,DF[,"piece"]),"count"]),
             black=sum(DF[grepl(blackPattern,DF[,"piece"]),"count"]),
             row.names=game_pgn)
}

CSTL <- function(X){
  if(grepl("O-O",X)){
    kingColumn <- "g"
    rookColumn <- "f"
    rookVacant <- "h"
  }
  if(grepl("O-O-O",X)){
    kingColumn <- "c"
    rookColumn <- "d"
    rookVacant <- "a"
  }
  if(grepl(whitePattern,X)){
    Row <- 1
  }
  if(grepl(blackPattern,X)){
    Row <- 8
  }
  list(king=paste(kingColumn,Row,sep=""),
       rook=paste(rookColumn,Row,sep=""),
       kingVacant=paste("e",Row,sep=""),
       rookVacant=paste(rookVacant,Row,sep=""))
}

queenPost. <- function(square,game_pgn){
  R <- rookPost.(square,game_pgn)
  B <- bishopPost.(square,game_pgn)
  sort(c(R,B))
}

kingPost. <- function(square,game_pgn){
  Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  KRange <- function(center=5){
    K <- (center-1):(center+1)
    K[0<K & K<=8]
  }
  Path <- expand.grid(letters[KRange(Col)],KRange(Row))
  Path <- paste(Path[,1],Path[,2],sep="")
  Path[Path!=square]
  Test <- function(toSquare,fromSquare=square){
    fromColor <- squareColor(fromSquare,game_pgn)
    if(!is.na(fromColor)){
      if(fromColor==whitePattern){colorPattern <- blackPattern}
      if(fromColor==blackPattern){colorPattern <- whitePattern}
    }else{colorPattern <- blackPattern}
    toColor <- squareColor(toSquare,game_pgn)
    grepl(colorPattern,position[game_pgn,toSquare]) |
      is.na(position[game_pgn,toSquare]) |
      grepl("phantom", position[game_pgn,toSquare])
  }
  Path <- Path[unlist(lapply(Path,Test))]
  Path[Path!=square]
}

quickInput <- function(pgn){
  lastRow <- row.names(position)[nrow(position)]
  # lastRow <- "C1600W_26...Qxa1"
  if(grepl("zero",lastRow)){move <- 1}else{
    move <- strsplit(lastRow,"_|\\.")[[1]]
    move <- as.numeric(move[grepl("^\\d+$",move)])
    if(grepl(blackPattern,lastRow)){
      move <- move+1
    }
  }
  newRow <- paste(gameName,"_",move,pgn,sep="")
  position <- rbind(position,newPosition(newRow,nrow(position))); 
  return(position)
}

upboard <- function(x){
  if(!(tolower(x) %in% c(1:8,letters[1:8]))){
    stop("invalid X coordinate")
  }
  if(tolower(x) %in% letters[1:8]){
    return(paste(tolower(x),1:8,sep=""))
  }
  if(x %in% 1:8){
    return(paste(letters[x],1:8,sep=""))
  }
}

linesToPgn <- function(pgnLines){
  
  # obtain pgnLines that contain chess moves; they need to be reformatted
  pgn <- Lines[grep("\\d\\..+[abcdefgh]\\d",Lines)]
  
  # collapse strings to prevent moves from being split from their corresponding integers
  pgn <- paste(pgn,collapse=" ")
  
  # split moves by space so they can be individually notated
  pgn <- strsplit(pgn," ")[[1]]
  
  # remove empty objects that may have accrued from double spacing or other issues
  pgn <- 
    pgn[grep("[[:alpha:]]",pgn)]
  
  # add 'move numbers' and the '...' to distinguish moves by black
  pgn[seq(2,length(pgn),2)] <- 
    paste(seq(1,length(pgn)/2),"...",pgn[seq(2,length(pgn),2)],sep="")
  
  # return vector
  pgn
}
