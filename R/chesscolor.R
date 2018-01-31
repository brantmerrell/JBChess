chesscolor <- function(pgn){
	if(grepl("(?<!\\.)\\.{3}(?!\\.)",pgn,perl=T)){return("black")}
	if(grepl("(?<!\\.)\\.{1}(?!\\.)",pgn,perl=T)){return("white")}
	return(NA)
}

# testing:
print(paste("one dot - expect white:", chesscolor("1.e3")))
print(paste("two dots - expect NA:",chesscolor("1..e6")))
print(paste("three dots - expect black:", chesscolor("1...e6")))
print(paste("four dots - expect NA:", chesscolor("1....e3")))
