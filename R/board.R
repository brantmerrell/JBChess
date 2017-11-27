board <- function (game_pgn = nrow(position), command = "View,return",wordSep = " ") {
	DF <- data.frame(matrix(position[game_pgn, ], nrow = 8, ncol = 8), 
		row.names = 8:1)
	colnames(DF) <- letters[1:8]
	DF <- apply(DF, c(1, 2), gsub, pattern = " ", replacement = wordSep)
	dimnames(DF) <- list(8:1, letters[1:8])
	if (grepl("View", command)) {
		View(DF)
	}
	if (grepl("return", command)) {
		return(as.matrix(DF))
	}
}
