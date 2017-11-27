pawnLeap <- function (game_pgn = nrow(position)) {
	square <- strsplit(row.names(position)[game_pgn], "\\.|_")[[1]]
	square <- square[length(square)]
	square <- gsub("[[:punct:]]", "", square)
	column <- strsplit(square, "")[[1]]
	row <- column[grepl("\\d", column)]
	column <- column[grepl("[abcdefgh]", column)]
	if (row == 5) {
		fromRow <- 7
		midRow <- 6
	}
	if (row == 4) {
		fromRow <- 2
		midRow <- 3
	}
	fromSquare <- paste(column, fromRow, sep = "")
	midSquare <- paste(column, midRow, sep = "")
	if (!row %in% c(4, 5)) {
		return(F)
	}
	else {
		isPawn <- grepl(pawnPattern, row.names(position)[game_pgn])
		didntCapture <- !grepl("x", row.names(position)[game_pgn])
		leapTo <- grepl("[abcdefg][45]", row.names(position)[game_pgn])
		leapFrom <- position[game_pgn, square] == position[lookBack(game_pgn, 
			1), fromSquare]
		midVacant <- is.na(position[lookBack(game_pgn, 1), midSquare])
		return(isPawn & didntCapture & leapTo & leapFrom & midVacant)
	}
}
