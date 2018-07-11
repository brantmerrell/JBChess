kingPost. <- function(square,game_pgn){
	Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
	Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
	KRange <- function(center=5){
		K <- (center-1):(center+1)
		K[0<K & K<=8]
	}
	Path <- expand.grid(letters[KRange(Col)],KRange(Row))
	Path <- paste(Path[,1],Path[,2],sep="")
	Path[Path!=square]
	Test <- function(toSquare,fromSquare=square){
		fromColor <- pieceColor(fromSquare,game_pgn)
		if(!is.na(fromColor)){
			if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
			if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
		}else{colorPattern <- chesspatterns$black}
		toColor <- pieceColor(toSquare,game_pgn)
		grepl(colorPattern,position[game_pgn,toSquare]) |
			is.na(position[game_pgn,toSquare]) |
			grepl("phantom", position[game_pgn,toSquare])
	}
	Path <- Path[unlist(lapply(Path,Test))]
	Path[Path!=square]
}
