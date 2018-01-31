# A function to list the squares and pieces between two squares of the chessboard. 

# It is called by the bishopPost., pawnPost., rookPost., and (by extension) queenPost. functions to determine the distance they can move along their ranks, files, and diagonals. 

# It uses the stop function when it receives incompatible inputs, so should rarely be called manually.

betweens <- function(square1,square2,game_pgn){
	Col1 <- which(letters==strsplit(square1,"")[[1]][1]) # convert square's column letter to a number
	Row1 <- as.numeric(strsplit(square1,"")[[1]][2]) # extract square's row number
	Col2 <- which(letters==strsplit(square2,"")[[1]][1]) # convert square2 column letter to a number
	Row2 <- as.numeric(strsplit(square2,"")[[1]][2]) # extract square2 row number
	if(length(Col1:Col2)!=length(Row1:Row2) & # check whether squares do NOT share a diagonal
			 !(0 %in% c(Col1-Col2,Row1-Row2))){	# check whether squares do NOT share a line
		# if the squares do not share a rank, file, or diagonal, stop the function
		stop("these two squares do not share rank, file, or diagnal")
	}
	btwns <- paste(letters[Col1:Col2],Row1:Row2,sep="") #list squares between the squares
	return(data.frame(squares=btwns,
										occupants=as.character(position[game_pgn,btwns]))) #retrieve occupants
}
