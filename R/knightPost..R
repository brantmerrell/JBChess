knightPost. <- function(square,game_pgn=nrow(position)){
	if(!grepl(chesspatterns$knight,position[game_pgn,square])){stop("this piece is not a knight")}
	Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
	Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
	NRange <- function(center,dist){ # create 1-dim function for plus & minus board distance
		dist <- unique(c(dist,dist*-1)) # distance = plus and minus displacement
		N <- expand.grid(center,dist) # combinations of center and displacements
		N <- apply(N,1,sum) # add center and displacements for 1-d location
		N[0<N & N<=8] # return only what exists within board boundaries
	}
	Path <- rbind(expand.grid(letters[NRange(Col,1)],NRange(Row,2)), # generate 1xCol and 2xRow
								expand.grid(letters[NRange(Col,2)],NRange(Row,1))) # generate 2xCol and 1xRow
	Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
	Test <- function(toSquare,fromSquare=square){
		testPattern <- ifelse(grepl(chesspatterns$black,position[game_pgn,fromSquare]),
													chesspatterns$white,chesspatterns$black)
		testPattern <- paste(testPattern,"phantom",sep="|")
		grepl(testPattern,position[game_pgn,toSquare]) |
			is.na(position[game_pgn,toSquare])
	}
	Path[unlist(lapply(Path,Test))]
}
