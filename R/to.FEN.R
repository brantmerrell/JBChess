# position_df <- positions
to.FEN <- function(position_df){
  pgn <- row.names(position_df)
  if(length(pgn)==1) {
    message("warning: single-row input may limit accuracy of castling and halfmove clock")
  }
  position_vec <- unlist(position_df[nrow(position_df),])
  if(sum(is.na(position_vec))==length(position_vec)) return("")
  enPassant <- grepl("phantom", position_vec, ignore.case = T)
	enPassant <- names(position_vec)[enPassant]
	if(length(enPassant)==0){enPassant <- "-"}
	
	ORD <- rep(letters[1:8],8)
	for(n in 1:8){
		m <- 8*n
		ORD[(m-7):m] <- paste(ORD[m:(m-7)], n, sep = "")
	}
	ORD <- rev(ORD)
	string <- as.vector(position_vec[ORD])
	string[is.na(string)] <- "1"
	string[string==""] <- "1"
	string[grepl(chesspatterns$white,string)] <- toupper(string[grepl(chesspatterns$white,string)])
	string[grepl(chesspatterns$black,string)] <- tolower(string[grepl(chesspatterns$black,string)])
	string <- gsub("WHITE |black ","",string)
	string[grepl(chesspatterns$knight,string)] <- sub("[Kk]","",string[grepl(chesspatterns$knight,string)])
	string <- unlist(lapply(string, substr, start = 1, stop = 1))
	string <- c(string[ c(1:8) ], "/", string[ c(9:16)], "/",
							string[c(17:24)], "/", string[c(25:32)], "/",
							string[c(33:40)], "/", string[c(41:48)], "/",
							string[c(49:56)], "/", string[c(57:64)])
	n <- 1
	while(n <= length(string)){
		if(grepl("\\d",string[n])){
			while(grepl("\\d",string[n+1])){
				string[n] <- as.character(sum(as.numeric(string[c(n,n+1)])))
				string <- string[-(n+1)]
			}
		}
		n <- n + 1
	}
	string <- paste(string, collapse = "")
	
	if(grepl(paste0("zero|",chesspatterns$black), pgn[length(pgn)])){
		string <- paste(string, "w")
	}else if(grepl(chesspatterns$white, pgn[length(pgn)])){
		string <- paste(string, "b")
	}else{stop("inconclusive color")}
	
	tests <- rep(NA, 4)
	names(tests) <- c("K", "Q", "k", "q")
	rows <- grepl("\\d\\.", row.names(position_df))
	if(sum(rows)==0){
	  tests[1:4] <- T
	}else{
	  temp <- unique(position_df[rows, c("a1","e1")])
	  tests["K"] <- nrow(temp)==1
	  temp <- unique(position_df[rows, c("h1","e1")])
	  tests["Q"] <- nrow(temp)==1
	  temp <- unique(position_df[rows, c("a8","e8")])
	  tests["k"] <-nrow(temp)==1
	  temp <- unique(position_df[rows, c("h8","e8")])
	  tests["q"] <-length(temp)==1
	  tests <- tests[tests]
	}
	
	if(length(tests)==0){
		tests <- "-"
	}else{
		tests <- paste(names(tests), collapse = "")
	}
	string <- paste(string, tests)
	string <- paste(string, enPassant)
	
	halfClock <- which(grepl("=[KQRNB]", pgn) | grepl("x", pgn))
	if(length(halfClock)=="0") halfClock <- "-"
	halfClock <- length(pgn)-max(halfClock)
	string <- paste(string,halfClock)
	
	fullMove <- row.names(position_df)[!grepl("empty|zero", row.names(position_df))]
	fullMove <- ceiling(length(fullMove)/2)
	string <- paste(string, fullMove)
  names(string) <- row.names(position_df)[nrow(position_df)]
	string
}
