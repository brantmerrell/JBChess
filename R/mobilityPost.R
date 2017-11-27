mobilityPost <- function (game_pgn = 2, piecePattern = "") {
	DF <- mobilityMap(game_pgn)
	DF <- DF[grepl(piecePattern, DF$piece), ]
	if (class(game_pgn) %in% c("numeric", "integer")) {
		game_pgn <- row.names(position)[game_pgn]
	}
	data.frame(white = sum(DF[grepl(whitePattern, DF[, "piece"]), 
		"count"]), black = sum(DF[grepl(blackPattern, DF[, "piece"]), 
		"count"]), row.names = game_pgn)
}
