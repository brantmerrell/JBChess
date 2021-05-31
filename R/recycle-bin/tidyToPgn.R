tidyToPgn <- function (tidyDF = ALL, n, string = "beta", lengthLimit = 2) {
	pgn <- as.vector(tidyDF[n, "pgn"])
	pgn <- strsplit(pgn, " ")[[1]]
	pgn <- pgn[grepl("[[:alpha:]]", pgn)]
	if (length(pgn) < lengthLimit) {
		return(cbind(gameID = n, pgn = NA))
	}
	else {
		blck <- paste("...", pgn[seq(2, length(pgn), 2)], sep = "")
		blck <- paste(seq(1, length(blck)), blck, sep = "")
		pgn[seq(2, length(pgn), 2)] <- blck
		return(cbind(gameID = paste(string, n, sep = ""), pgn))
	}
}
