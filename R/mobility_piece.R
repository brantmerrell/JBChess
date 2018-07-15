# DEBUG ISSUE:
square = "b3"
position_vec = positions["30...b2",]
piece = NULL
mobility_piece <- function(square, position_vec=NULL, piece=NULL){
  
  # test position_vec and piece for null
  TEST <- unlist(lapply(list(position_vec,piece), is.null))

  # position_vec and piece should not both be NULL
  if(sum(TEST) == 2) return(NULL)

  # if position_vec is null, return mobility of piece on empty board
  if(is.null(position_vec)){
    Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
    Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
    
    # if the piece is a pawn:
    if(grepl(chesspatterns$pawn,piece)){
      
      # if the pawn's color has not yet been defined, define it as nothing. Why?
      if(!exists("color")){color <- ""}
      
      # if object "direction" already exists in environment, ignore it
      if(exists("direction")) rm(direction)
      
      # if the pawn is black, it moves down the board:
      if(grepl(chesspatterns$black,piece)){direction <- -1} 
      
      # if the pawn is white, it moves up the board:
      if(grepl(chesspatterns$white,piece)){direction <- 1} 
      
      # if the pawn color is unspecified, return both directions
      if(!exists("direction")){direction <- c(1,-1)}
      
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
      
      # if two directions exist, add question marks
      if(length(direction)==2) Path <- paste0(Path,"?")
      
      # return all squares to which the pawn can move on an empty board:
      return(Path)
    }
    if(grepl(chesspatterns$bishop,piece)){ # check whether the piece is a bishop
      B <- function(x){ # define a function that takes as input any row, 
        V <- c(x+Col-Row,Col+Row-x) # already knows bishop location, 
        paste(letters[x],V[0<V & V<=8],sep="") 
        # and returns squares on that row where the bishop can move
      }
      Path <- unique(unlist(sapply(1:8,B))) # input all rows on the board
    }
    if(grepl(chesspatterns$knight,piece)){ # check whether the piece is a knight
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
    if(grepl(chesspatterns$rook,piece)){ # check whether the piece is a rook
      Path <- unique(c(paste(letters[Col],1:8,sep=""), # generate squares along column
                       paste(letters[1:8],Row,sep=""))) # generate squares along row
    }
    if(grepl(chesspatterns$queen,piece)){ # check whether the piece is a Queen
      # replicate bishop function:
      B <- function(x){ 
        V <- c(x+Col-Row,Col+Row-x) 
        paste(letters[x],V[0<V & V<=8],sep="")
      }
      Path <- c(unique(unlist(sapply(1:8,B))), # generate diagonals
                unique(c(paste(letters[Col],1:8,sep=""), # generate squares along column
                         paste(letters[1:8],Row,sep="")))) # generate squares along row
    }
    if(grepl(chesspatterns$king,piece)){ # check whether the piece is a King
      KRange <- function(center){ # create 1-dim function for plus to minus board distance (1) 
        K <- (center-1):(center+1) # broaden center in two directions
        K[0<K & K<=8] # return only results within board boundaries
      }
      Path <- expand.grid(letters[KRange(Col)], # generate all combinations of King's column range
                          KRange(Row)) # generate all combinations of King's row range
      Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
    }
    return(Path[Path!=square]) # a piece can't move to the square on which it already resides!
  }
  
  # if position_vec is a data frame, convert to a named vector
  if(class(position_vec)=="data.frame"){
    position_vec <- unlist(position_vec[nrow(position_vec),])
  }
  
  # if position_vec is not named, throw error
  if(is.null(names(position_vec))) { stop("position_vec must be named vector") }
  
  # obtain piece from position_vec & square
  piece <- position_vec[square]
  
  # test whether piece is a pawn
  if(grepl(chesspatterns$pawn,piece)){
    # specify direction for black pawns
    if(grepl(chesspatterns$black,position_vec[square])){direction <- -1}
    
    # specify direction for white pawns
    if(grepl(chesspatterns$white,position_vec[square])){direction <- 1}
    
    # specify double direction for unspecified pawns
    if(!grepl(paste(chesspatterns$black,chesspatterns$white, sep="|"), position_vec[square])){
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
      ifelse((grepl(chesspatterns$black,position_vec[fromSquare]) & # black attacking pience & 
                grepl(chesspatterns$white,position_vec[AttSquare])) | # white defending piece OR
               (grepl(chesspatterns$white,position_vec[fromSquare]) & # white attacking piece & 
                  grepl(chesspatterns$black,position_vec[AttSquare])), # black defending piece
             # . . . return TRUE, otherwise FALSE
             T,F)
    }
    
    # create a function to test whether the pawn can move to a non-attacking square:
    normTest <- function(normSquare,fromSquare=square){
      
      # create path to find out whether pawn would be required hop over or capture any pieces:
      DF <- betweens(normSquare,fromSquare,position_vec)
      
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
  # test whether piece is a knight
  if(grepl(chesspatterns$knight,piece)){
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
      testPattern <- ifelse(grepl(chesspatterns$black,position_vec[fromSquare]),
                            chesspatterns$white,chesspatterns$black)
      testPattern <- paste(testPattern,"phantom",sep="|")
      grepl(testPattern,position_vec[toSquare]) |
        is.na(position_vec[toSquare])
    }
    return(Path[unlist(lapply(Path,Test))])
  }
  # test whether piece is a bishop or queen (both require generating bishop mobility)
  if(grepl(chesspatterns$bishop,piece) | grepl(chesspatterns$queen, piece)){
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
      fromColor <- pieceColor(fromSquare,position_vec)
      
      # store the color that the moving piece can capture
      if(!is.na(fromColor)){
        if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
        if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
        
        # if the moving piece has no listed color, allow to capture either color
      }else{colorPattern <- paste(chesspatterns$white,chesspatterns$black,sep="|")}
      
      # define the color of the destination square
      toColor <- pieceColor(toSquare,position_vec)
      
      # if the destination square is a phantom pawn, set its color as NA
      if(grepl("phantom",position_vec[toSquare])){toColor <- NA}
      
      # map out pieces and squares between starting square and destination square
      DF <- betweens(toSquare,fromSquare,position_vec)
      
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
    B <- Path[unlist(lapply(Path,Test))]
    if(grepl(chesspatterns$bishop, piece)) return(B)
  }
  # test whether piece is a rook (both require generating rook mobility)
  if(grepl(chesspatterns$rook,piece) | grepl(chesspatterns$queen, piece)){
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
      fromColor <- pieceColor(fromSquare,position_vec)
      
      # store the color that can be captured by moving piece
      if(!is.na(fromColor)){
        if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
        if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
        
        # if moving piece color is unspecified, default to white
      }else{colorPattern <- chesspatterns$black}
      
      # define the color of the destination square
      toColor <- pieceColor(toSquare,position_vec)
      
      # if the destination square is a phantom pawn, set its color as NA
      if(grepl("phantom",position_vec[toSquare])){toColor <- NA}
      
      # map out squares and pieces between start and destination squares
      DF <- betweens(toSquare,fromSquare,position_vec)
      
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
    R <- Path[unlist(lapply(Path,Test))]
    if(grepl(chesspatterns$rook,piece)) return(R)
  }
  # test whether piece is a queen
  if(grepl(chesspatterns$queen,piece)){
    return(c(R,B))
  }
  # test whether piece is a king
  if(grepl(chesspatterns$king,piece)){
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
      fromColor <- pieceColor(fromSquare,position_vec)
      if(!is.na(fromColor)){
        if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
        if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
      }else{colorPattern <- chesspatterns$black}
      toColor <- pieceColor(toSquare,position_vec)
      grepl(colorPattern,position_vec[toSquare]) |
        is.na(position_vec[toSquare]) |
        grepl("phantom", position_vec[toSquare])
    }
    Path <- Path[unlist(lapply(Path,Test))]
    return(Path[Path!=square])
  }
}
