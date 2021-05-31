neighborPassant <- function(game_pgn=nrow(position)){
	square <- strsplit(row.names(position)[game_pgn],"\\.|_")[[1]]
	square <- square[length(square)]
	square <- gsub("[[:punct:]]","",square)
	square <- strsplit(square,"")[[1]]
	square <- paste(square[length(square)-(1:0)],collapse="")
	piece <- position[game_pgn,square]
	Column <- strsplit(square,"")[[1]]
	Row <- Column[grepl("\\d",Column)]
	Column <- Column[grepl("[abcdefgh]",Column)]
	neighborCol <- letters[which(letters==Column)+c(-1,1)]
	neighborSquare <- paste(neighborCol,Row,sep="")
	neighborPiece <- position[game_pgn,neighborSquare]
	arePawns <- grepl("pawn",piece) & grepl("pawn",neighborPiece)
	diffColors <- (grepl(chesspatterns$black,piece) & grepl(chesspatterns$white,neighborPiece)) |
	grepl(chesspatterns$white,piece) & grepl(chesspatterns$black, neighborPiece)
	passantRows <- grepl("[45]",square) & grepl("[45]",neighborSquare)
	ifelse(F %in% c(arePawns, diffColors, passantRows),return(F),return(T))
}
