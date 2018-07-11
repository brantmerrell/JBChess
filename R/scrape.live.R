invisible(library(jsonlite))
scrape.live <- function(live.link=NULL, id=NULL){
	# if id is not provided, extract from link
	if(length(id)!=1){
		# extract game id from link
		id <- gsub("^.+/", "", live.link)

	# if id is provided, construct link from id
	}else{
		# combine with "/", but not with "//"
		live.link <- file.path("https://www.chess.com/live/game", id)
	}
	# Get html code frokm link
	Html <- readLines(live.link) # get link

	# test to ensure scraping pattern & procedure are compatible with Html
	if(sum(grepl(paste(id,",&quot;",sep=""),Html))!=1){
		# return empty data if incompatible
		return(data.frame(White="",Black="", stringsAsFactors = F))

	# Remove all Html except the part with the data
	}else{Html <- Html[grepl(paste(id,",&quot;",sep=""),Html)]}

	# convert "&quot;" to quotes for conversion to json object
	Json <- gsub('&quot;','"', Html)

	# remove enters and trailing comma for conversion to json
	Json <- gsub("\r|\n|,$","",Json) 

	# use jsonlite (not RJSONIO) to convert to json object
	Json <- jsonlite::fromJSON(Json) 

	# define pgn object (is currently a string, but contains multiple objects to extract)
	pgn <- Json$pgn 

	# remove Json pgn object (it's already defined)
	Json$pgn <- NULL 

	# This json object contains *only* the website-specific data about the game
	# Other data about the game is stored as json arrays in the pgn string
	# create matrix of this json object, convert to data frame:
	DF1 <- matrix(Json, ncol = length(Json), dimnames=list(NULL, names(Json)))
	DF1 <- data.frame(DF1, stringsAsFactors=F)

	# split pgn into elements (to separate move data from game data)
	pgn <- strsplit(pgn, "\\r|\\n")[[1]]

	# remove empty pgn spaces created by split
	pgn <- pgn[grepl(".",pgn)] 

	# extract starting FEN pattern for PGN (or it will be converted to string "1")
	FEN0 <- pgn[grepl("FEN",pgn)]

	# define non-positional game data to be converted to data frame
	DF2 <- pgn[grepl("^\\[.+\\]$",pgn)] 

	# define pgn as exclusively positional data and combine to single string
	pgn <- paste(pgn[!grepl("^\\[",pgn)],collapse="")

	# Convert non-positional data to matrix (from format [VariableName "Value"])
	DF2 <- matrix(sub('^.+ ', '', DF2),nrow=1, byrow=T, dimnames=list(NULL, gsub('".+$','', DF2)))
	DF2 <- data.frame(DF2, stringsAsFactors=F)

	# remove brackets and quotes from DF2 strings
	DF2 <- apply(DF2, 2, gsub, pattern = '\\[|\\]|\\"', replacement='')

	# apply function converted DF2 to class character. Convert to matrix.
	DF2 <- matrix(DF2, nrow=1, byrow=T, dimnames=list(NULL, names(DF2)))

	# Convert to data frame
	DF2 <- as.data.frame(DF2, stringsAsFactors=F)

	# remove punctuation, spaces, and Xs (at string beginnings) from DF2 names
	colnames(DF2) <- gsub("[[:punct:]]|^X", "", colnames(DF2))

	# combine both matrices into one; delete them
	DF <- data.frame(cbind(DF1,DF2),stringsAsFactors=F);rm(DF1,DF2)
	
	# convert any columns of class "list" to class "character"
	for(m in 1:ncol(DF)){
	  if(class(DF[,m])=="list"){DF[,m] <- as.character(DF[,m])}
	}

	# add positional game data (pgn and FEN @ move zero) to data frame
	DF <- cbind(DF, pgn, FEN0, stringsAsFactors=F)

	# Do not return FEN because it's just the string "1"
	DF[,!grepl("^FEN$",colnames(DF))]

}

