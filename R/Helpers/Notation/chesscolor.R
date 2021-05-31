chesscolor <- function(pgn){
	if(grepl("(?<!\\.)\\.{3}(?!\\.)",pgn,perl=T)){return("black")}
	if(grepl("(?<!\\.)\\.{1}(?!\\.)",pgn,perl=T)){return("white")}
	return(NA)
}
