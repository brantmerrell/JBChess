pieceColor <- function(square="a1",game_pgn=2){
	patternVec <- c(blackPattern,whitePattern)
	patternTest <- unlist(lapply(patternVec, grepl, x = position[game_pgn,square]))
	ifelse(sum(patternTest)==0, return(NA), 
				 return(patternVec[patternTest]))
}
