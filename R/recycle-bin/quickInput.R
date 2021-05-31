quickInput <-	function (pgn) {
		lastRow <- row.names(position)[nrow(position)]
		if (grepl("zero", lastRow)) {
				move <- 1
		}
		else {
				move <- strsplit(lastRow, "_|\\.")[[1]]
				move <- as.numeric(move[grepl("^\\d+$", move)])
				if (grepl(chesspatterns$black, lastRow)) {
						move <- move + 1
				}
		}
		newRow <- paste(gameName, "_", move, pgn, sep = "")
		position <- rbind(position, newPosition(newRow, nrow(position)))
		return(position)
}
