mobilityMap <- function(game_pgn){
	DF <- pieceCount(game_pgn)
	for (n in 1:nrow(DF)) {
		options1 <- piecePost.(as.character(DF$square[n]), game_pgn)
		DF[n, "options"] <- paste(options1, collapse = ";")
		DF[n, "count"] <- ifelse(grepl(".\\d", DF[n, "options"]), 
			length(options1), 0)
#		vacant <- is.na(position[game_pgn, options1]) | grepl("phantom", 
#			position[game_pgn, options1])
#		DF[n, ]
	}
	DF
}
