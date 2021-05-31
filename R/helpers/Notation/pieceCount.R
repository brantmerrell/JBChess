source("R/to.PGN.R")
pieceCount <- function(position_vec){
  
  if(class(position_vec)=="data.frame"){
    position_vec <- unlist(position_vec[nrow(position_vec),])
  }
	
	# identify which part of the board is occupied
	Col <- !is.na(position_vec)
	Col <- Col & ""!=position_vec
	
	# identify the pieces on the occupied portion of the
	
	if(!sum(Col)==1){
		square <- names(position_vec)[Col]
		piece <- position_vec[Col]
		DF <- data.frame(square=names(position_vec[Col]),
										 piece=as.character(as.matrix(position_vec[Col])),
										 stringsAsFactors = F)
	}else{
		DF <- data.frame(square=names(position_vec[Col]),
										 piece=piece, 
										 stringsAsFactors = F)
	}
  DF$pgn <- to.PGN(paste0(DF$piece, DF$square))
	DF
}
