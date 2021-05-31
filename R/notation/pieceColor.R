pieceColor <- function(square, position_vec){
	patternVec <- c(chesspatterns$black,chesspatterns$white)
	patternTest <- unlist(lapply(patternVec, grepl, x = position_vec[square]))
	ifelse(sum(patternTest)==0, return(NA), 
				 return(patternVec[patternTest]))
}
