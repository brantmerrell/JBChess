pieceColor <- function(square="a1",game_pgn=2){
	patternVec <- c(chesspatterns$black,chesspatterns$white)
	patternTest <- unlist(lapply(patternVec, grepl, x = position[game_pgn,square]))
	ifelse(sum(patternTest)==0, return(NA), 
				 return(patternVec[patternTest]))
}
