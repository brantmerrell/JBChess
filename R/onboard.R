onboard <- list(
  pawn = function(square, game_pgn){
    # specify direction for black pawns
    if(grepl(chesspatterns$black,position[game_pgn,square])){direction <- -1}
    
    # specify direction for white pawns
    if(grepl(chesspatterns$white,position[game_pgn,square])){direction <- 1}
    
    # specify double direction for unspecified pawns
    if(!grepl(paste(chesspatterns$black,chesspatterns$white, sep="|"), position[game_pgn,square])){
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
      ifelse((grepl(chesspatterns$black,position[game_pgn,fromSquare]) & # black attacking pience & 
                grepl(chesspatterns$white,position[game_pgn,AttSquare])) | # white defending piece OR
               (grepl(chesspatterns$white,position[game_pgn,fromSquare]) & # white attacking piece & 
                  grepl(chesspatterns$black,position[game_pgn,AttSquare])), # black defending piece
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
  },
  knight = function(square, game_pgn){
    if(!grepl(chesspatterns$knight,position[game_pgn,square])){stop("this piece is not a knight")}
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
      testPattern <- ifelse(grepl(chesspatterns$black,position[game_pgn,fromSquare]),
                            chesspatterns$white,chesspatterns$black)
      testPattern <- paste(testPattern,"phantom",sep="|")
      grepl(testPattern,position[game_pgn,toSquare]) |
        is.na(position[game_pgn,toSquare])
    }
    Path[unlist(lapply(Path,Test))]
  },
  bishop = function(square,game_pgn){
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
      fromColor <- pieceColor(fromSquare,game_pgn)
      
      # store the color that the moving piece can capture
      if(!is.na(fromColor)){
        if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
        if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
        
        # if the moving piece has no listed color, allow to capture either color
      }else{colorPattern <- paste(chesspatterns$white,chesspatterns$black,sep="|")}
      
      # define the color of the destination square
      toColor <- pieceColor(toSquare,game_pgn)
      
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
  },
  rook = function(square, game_pgn){
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
      fromColor <- pieceColor(fromSquare,game_pgn)
      
      # store the color that can be captured by moving piece
      if(!is.na(fromColor)){
        if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
        if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
        
        # if moving piece color is unspecified, default to white
      }else{colorPattern <- chesspatterns$black}
      
      # define the color of the destination square
      toColor <- pieceColor(toSquare,game_pgn)
      
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
  },
  queen = function(square, game_pgn){
    R <- onboard$rook(square,game_pgn)
    B <- onboard$bishop(square,game_pgn)
    sort(c(R,B))
  },
  king = function(square, game_pgn){
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
      fromColor <- pieceColor(fromSquare,game_pgn)
      if(!is.na(fromColor)){
        if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
        if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
      }else{colorPattern <- chesspatterns$black}
      toColor <- pieceColor(toSquare,game_pgn)
      grepl(colorPattern,position[game_pgn,toSquare]) |
        is.na(position[game_pgn,toSquare]) |
        grepl("phantom", position[game_pgn,toSquare])
    }
    Path <- Path[unlist(lapply(Path,Test))]
    Path[Path!=square]
  }
)
