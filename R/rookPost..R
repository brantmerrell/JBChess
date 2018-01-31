rookPost. <- function(square,game_pgn){
	
	# isolate column and rows of square
	Col <- which(letters==strsplit(square,"")[[1]][1]) 
	Row <- as.numeric(strsplit(square,"")[[1]][2])
	
	# generate rook path for empty board
	Path <- unique(c(paste(letters[Col],1:8,sep=""),paste(letters[1:8],Row,sep="")))
	
	# remove rook's current square from path
	Path <- sort(Path[Path!=square])
	
	# create test to eliminate faulty squares from path
	Test <- function(toSquare,fromSquare=square){
		
		# determine the color of the moving piece
		fromColor <- pieceColor(fromSquare,game_pgn)
		
		# store the color that can be captured by moving piece
		if(!is.na(fromColor)){
			if(fromColor==whitePattern){colorPattern <- blackPattern}
			if(fromColor==blackPattern){colorPattern <- whitePattern}
			
		# if moving piece color is unspecified, default to white
		}else{colorPattern <- blackPattern}
		
		# define the color of the destination square
		toColor <- pieceColor(toSquare,game_pgn)
		
		# if the destination square is a phantom pawn, set its color as NA
		if(grepl("phantom",position[game_pgn,toSquare])){toColor <- NA}
		
		# map out squares and pieces between start and destination squares
		DF <- betweens(toSquare,fromSquare,game_pgn)
		
		# test whether squares between start and destination are clear:
		T1 <- (sum(is.na(DF$occupants[
			
			# (exclude start and destination squares)
			-c(1,nrow(DF))]) | 
				
				# add squares with phantom pawns as vacant
				grepl("phantom",DF$occupants[-c(1,nrow(DF))]))
					 
					 # make sure the number of clear squares equals the exclusive 'between distance'
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
