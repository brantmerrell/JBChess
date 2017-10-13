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
