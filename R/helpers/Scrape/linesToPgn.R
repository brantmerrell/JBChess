linesToPgn <- function(pgnLines){
	
	# obtain pgnLines that contain chess moves; they need to be reformatted
	pgn <- Lines[grep("\\d\\..+[abcdefgh]\\d",Lines)]
	
	# collapse strings to prevent moves from being split from their corresponding integers
	pgn <- paste(pgn,collapse=" ")
	
	# split moves by space so they can be individually notated
	pgn <- strsplit(pgn," ")[[1]]
	
	# remove empty objects that may have accrued from double spacing or other issues
	pgn <- 
		pgn[grep("[[:alpha:]]",pgn)]
	
	# add 'move numbers' and the '...' to distinguish moves by black
	pgn[seq(2,length(pgn),2)] <- 
		paste(seq(1,length(pgn)/2),"...",pgn[seq(2,length(pgn),2)],sep="")
	
	# return vector
	pgn
}
