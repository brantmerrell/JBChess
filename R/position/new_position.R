# new_pgn <- workFrame[n,"pgn"]
# position_vec <- positions
new_position <- function(new_pgn, position_vec){
  
  if(class(position_vec)=="data.frame"){
    position_vec <- unlist(position_vec[nrow(position_vec),])
  }
  
  # determine whether the move is a 'castle', in which king and rook both move:
  castle <- grepl(chesspatterns$castle,new_pgn)
  
  # determine whether the move is a pawn promotion:
  promotion <- grepl("[QRBN][+#]?$",new_pgn)
  
  # determine whether a black or a white piece is moving:
  color <- ifelse(grepl(chesspatterns$black,new_pgn),"black","white")
  
  # determine which move of the game is being assessed:
  move <- strsplit(new_pgn,"\\.|_")[[1]]
  move <- as.numeric(move[grep("^\\d+$",move)])
  move <- move[length(move)]
  
  # if this move is not a 'castle' or 'promotion',
  if(!castle & !promotion){
    
    # obtain the full name of the moving piece, including its color:
    piece <- paste(color,pgnPiece(new_pgn))
    
    # extract the name of the square to which the piece is moving:
    endSquare <- gsub("[[:punct:]]$","",new_pgn)
    endSquare <- strsplit(endSquare,"")[[1]]
    endSquare <- paste(endSquare[(length(endSquare)-1):length(endSquare)],collapse="")
    
    # to determine the vacant square, 
    # first list pieces that match the type and color of the moving piece:
    workFrame <- pieceCount(position_vec)
    workFrame <- workFrame[grepl(color,workFrame$piece),]
    workFrame <- workFrame[grepl(tolower(piece),tolower(workFrame$piece)),]
    
    # list the potential destinations of all pieces listed above,
    workList <- lapply(workFrame$square, mobility_piece, position_vec=position_vec)
    
    # name the list of potential destinations based on potential starting points: 
    names(workList) <- workFrame$square
    
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
    newPosition <- position_vec
    
    # define the rowname of the new position as the new input pgn:
    newPosition <- data.frame(matrix(newPosition, nrow = 1, 
                                     dimnames = list(new_pgn,names(newPosition))),
                              stringsAsFactors = F)
    
    # remove the moving piece from its starting point:
    newPosition[startSquare] <- NA
    
    # add the moving point to its destination point:
    newPosition[endSquare] <- piece
    
    # if moving piece is pawn and destination point is phantom, remove concrete pawn:
    if(grepl(chesspatterns$pawn,piece) & grepl("phantom",position_vec[endSquare])){
      Col <- which(letters==strsplit(endSquare,"")[[1]][1]) # identify the column
      if(grepl("^.6$",endSquare)){
        concretePawn <- paste(letters[Col],"5",sep="")
      }
      if(grepl("^.3$",endSquare)){
        concretePawn <- paste(letters[Col],"4",sep="")
      }
      newPosition[concretePawn] <- NA
    }
    
    # detect and remove any phantom pawns, because they only exist for one move:
    phanP <- !is.na(newPosition) & grepl("phantom",newPosition)
    # phanP <- unlist(lapply(newPosition,grepl,pattern="phantom"))
    newPosition[phanP & !is.na(newPosition)] <- NA
    
    # if piece was a pawn and it moved two spaces, create 'phantom pawn' for potential en passant
    if(grepl(chesspatterns$pawn,piece) & grepl("2|7",startSquare) & grepl("4|5",endSquare)){
      Col <- which(letters==strsplit(startSquare,"")[[1]][1]) # identify the column
      if(color=="black"){phantomSquare <- paste(letters[Col],"6",sep="")}
      if(color=="white"){phantomSquare <- paste(letters[Col],"3",sep="")}
      newPosition[phantomSquare] <- paste("phantom",color,"pawn")
    }
  }
  
  # if the move is a castle,
  if(castle){
    
    # the moving pieces consist of a king and a rook: 
    piece <- paste(chesspatterns$king,chesspatterns$rook,sep="|")
    
    # use CSTL() to define the destination squares of the king and rook:
    endSquare <- unlist(CSTL(new_pgn)[c("king","rook")])
    
    # use CSTL() to define the starting squares of the king and rook:
    startSquare <- unlist(CSTL(new_pgn)[c("kingVacant","rookVacant")])
    
    # generate a new position from the old position:
    newPosition <- position_vec
    
    # name the new position as the new input pgn:
    newPosition <- matrix(newPosition, nrow = 1, dimnames = list(new_pgn,names(newPosition)))
    
    # remove the King and Rook from their squares:
    newPosition[1,startSquare] <- NA
    
    # place the King and Rook on their destination squares:
    newPosition[1,endSquare] <- paste(color,names(endSquare))
    
    newPosition <- matrix(newPosition, nrow = 1, dimnames = list(new_pgn, names(newPosition)))
    
    newPosition <- as.data.frame(newPosition, stringsAsFactors = F)
    
    colnames(newPosition) <- names(position_vec)
  }
  
  # if the move is a promotion:
  if(promotion){
    
    # obtain the full name of the resulting piece, including its color:
    piece <- paste(color,pgnPiece(new_pgn))
    
    # extract the name of the square to which the piece is moving:
    endSquare <- strsplit(new_pgn,"[_\\.]")[[1]]
    if(grepl("_",new_pgn)){
      endSquare <- endSquare[-1]
    }
    endSquare <- endSquare[grepl("[[:lower:]]\\d=?[QRBN].?$",endSquare)]
    endSquare <- gsub("[[:upper:]]|[[:punct:]]","",endSquare)
    endSquare <- paste(tail(strsplit(endSquare,"")[[1]],2),collapse="")
    
    # to determine the vacant square, 
    # first list pawns that match working color:
    workFrame <- pieceCount(position_vec = position_vec)
    workFrame <- workFrame[grepl(color,workFrame$piece),]
    workFrame <- workFrame[grepl("pawn",tolower(workFrame$piece)),]
    
    # list the potential destinations of the pawns:
    workList <- lapply(as.character(workFrame$square), mobility_piece, position_vec=position_vec)
    
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
    startSquare <- names(workList)[startSquare]
    
    # define the new position as the old position, modify later:
    newPosition <- position_vec
    
    # define the rowname of the new position as the new input pgn:
    # names(newPosition) <- new_pgn
    
    # remove the moving piece from its starting point:
    newPosition[startSquare] <- NA
    
    # add the moving point to its destination point:
    newPosition[endSquare] <- piece
  }
  
  newPosition <- matrix(newPosition, nrow = 1, dimnames = list(new_pgn, names(newPosition)))
  
  newPosition <- as.data.frame(newPosition, stringsAsFactors = F)
  
  colnames(newPosition) <- names(position_vec)
  
  # return the new position:
  newPosition
}
