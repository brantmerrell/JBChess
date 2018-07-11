bishopPost. <- function(square,game_pgn){
	
	# identify row and column:
	Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
	Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
	
	# list the 'cartesian first quadrant' squares: 
	Lim <- min(length(Col:8),length(Row:8))
	Path <- paste(letters[Col:8][1:Lim],(Row:8)[1:Lim],sep="")
	
	# add the 'cartesian third quadrant' squares:
	Lim <- min(length(Col:1),length(Row:1))
	Path <- c(Path,paste(letters[Col:1][1:Lim],(Row:1)[1:Lim],sep=""))
	
	# add the 'cartesian fourth quadrant' squares:
	Lim <- min(length(Col:8),length(Row:1))
	Path <- c(Path,paste(letters[Col:8][1:Lim],(Row:1)[1:Lim],sep=""))
	
	# add the 'cartesian second quadrant' squares:
	Lim <- min(length(Col:1),length(Row:8))
	Path <- c(Path,paste(letters[Col:1][1:Lim],(Row:8)[1:Lim],sep=""))
	
	# because pieces cannot move to a square they occupy, remove occupied square from list:
	Path <- Path[Path!=square]
	
	# remove duplicate squares
	Path <- sort(unique(Path))
	
	# create a test indicating whether destination is compatible:
	Test <- function(toSquare,fromSquare=square){
		
		# isolate the color of moving piece
		fromColor <- pieceColor(fromSquare,game_pgn)
		
		# store the color that the moving piece can capture
		if(!is.na(fromColor)){
			if(fromColor==chesspatterns$white){colorPattern <- chesspatterns$black}
			if(fromColor==chesspatterns$black){colorPattern <- chesspatterns$white}
			
			# if the moving piece has no listed color, allow to capture either color
		}else{colorPattern <- paste(chesspatterns$white,chesspatterns$black,sep="|")}
		
		# define the color of the destination square
		toColor <- pieceColor(toSquare,game_pgn)
		
		# if the destination square is a phantom pawn, set its color as NA
		if(grepl("phantom",position[game_pgn,toSquare])){toColor <- NA}
		
		# map out pieces and squares between starting square and destination square
		DF <- betweens(toSquare,fromSquare,game_pgn)
		
		# test whether 'between squares' are vacant by taking the sum of vacant squares,
		T1 <- (sum(is.na(DF$occupants[
			
			# (excluding start and end squares)
			-c(1,nrow(DF))]) |
				
				# adding phantom squares as vacant,
				grepl("phantom",DF$occupants[-c(1,nrow(DF))]))
			
			# and testing whether this sum equals between squares (exclusive of start & dest)
			==nrow(DF)-2)
		
		# test whether destination square is a piece that can be captured by moving piece
		T2a <- grepl(colorPattern,DF[DF$squares==toSquare,"occupants"])
		
		# test whether destination square is vacant or phantom
		T2b <- is.na(DF[DF$squares==toSquare,"occupants"]) | 
			grepl("phantom",DF[DF$squares==toSquare,"occupants"])
		
		# summarize whether moving piece can travel to and arrive on destination 
		T1 & (T2a | T2b)
	}
	Path[unlist(lapply(Path,Test))]
}
