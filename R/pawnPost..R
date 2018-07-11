pawnPost. <- function(square,game_pgn=nrow(position)){
	
	# specify direction for black pawns
	if(grepl(chesspatterns$black,position[game_pgn,square])){direction <- -1}
	
	# specify direction for white pawns
	if(grepl(chesspatterns$white,position[game_pgn,square])){direction <- 1}
	
	# specify double direction for unspecified pawns
	if(!grepl(paste(chesspatterns$black,chesspatterns$white, sep="|"), position[game_pgn,square])){
		direction <- c(1,-1)
	}
	
	# identify the corresponding number to the column letter
	Col <- which(letters==strsplit(square,"")[[1]][1]) 
	
	# identify the column row
	Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
	
	# identify the columns which the pawn can attack:
	AttRange <- c((Col-1),(Col+1))
	
	# remove any 'attack columns' that do not exist on the board:
	AttRange <- AttRange[0<AttRange & AttRange<=8]
	
	# use the pawn's direction to identify the row it can attack:
	AttSq <- paste(letters[AttRange],Row+direction,sep="")
	
	# deduce whether the pawn has moved based on its color and row:
	ifelse((direction==-1 & Row==7)|(direction==1 & Row==2),
				 
				 # if the pawn has moved, it has the option of moving two spaces:
				 distance <- 1:2,
				 
				 # if the pawn has not moved, it can only move one space:
				 distance <- 1)
	
	# obtain the pawn's non-attack destinations by adding its row to its distance and direction:
	normSq <- paste(letters[Col],Row+(distance*direction),sep="")
	
	# create a function to test whether an attackable piece is on a square:
	AttTest <- function(AttSquare,fromSquare=square){
		
		# if the attacking and defending pieces are different colors... 
		ifelse((grepl(chesspatterns$black,position[game_pgn,fromSquare]) & # black attacking pience & 
							grepl(chesspatterns$white,position[game_pgn,AttSquare])) | # white defending piece OR
						 (grepl(chesspatterns$white,position[game_pgn,fromSquare]) & # white attacking piece & 
								grepl(chesspatterns$black,position[game_pgn,AttSquare])), # black defending piece
					 # . . . return TRUE, otherwise FALSE
					 T,F)
	}
	
	# create a function to test whether the pawn can move to a non-attacking square:
	normTest <- function(normSquare,fromSquare=square){
		
		# create path to find out whether pawn would be required hop over or capture any pieces:
		DF <- betweens(normSquare,fromSquare,game_pgn)
		
		# remove pawn's current location from generated path:
		DF <- DF[DF$squares!=fromSquare,]
		
		# test whether each space is clear:
		clear <- unlist(lapply(DF$occupants,is.na))
		
		# return logical on whether all spaces are clear:
		sum(clear)==nrow(DF)
	}
	
	# remove any attack squares that do not pass the pawn attack test:
	AttSq <- AttSq[unlist(lapply(AttSq,AttTest))]
	
	# remove any non-attack squares that do not pass the "clear path" test:
	normSq <- normSq[unlist(lapply(normSq,normTest))]
	
	# return all potential destinations, or if none exist, return NA
	ifelse(0<length(c(normSq,AttSq)),
				 return(c(normSq,AttSq)),
				 return(NA))
}
