to.FEN <- function(position_vec, pgn_history){
  if(class(position_vec)=="data.frame"){
    position_vec <- unlist(position_vec[nrow(position_vec),])
  }
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
	if(grepl(chesspatterns$black, pgn_history[length(pgn_history)])){
		string <- paste(string, "w")
	}else{
		string <- paste(string, "b")
	}
	tests <- rep(NA, 4)
	names(tests) <- c("K", "Q", "k", "q")
	# temp <- unique(as.vector(as.matrix(position[game,c("a1","e1")])))
	# tests["K"] <- length(temp)==2
	# temp <- unique(as.vector(as.matrix(position[game,c("h1","e1")])))
	# tests["Q"] <- length(temp)==2
	# temp <- unique(as.vector(as.matrix(position[game,c("a8","e8")])))
	# tests["k"] <- length(temp)==2
	# temp <- unique(as.vector(as.matrix(position[game,c("h8","e8")])))
	# tests["q"] <- length(temp)==2
	# tests <- tests[tests]
	if(length(tests)==0){
		tests <- "-"
	}else{
		tests <- paste(names(tests), collapse = "")
	}
	string <- paste(string, tests)
	string <- paste(string, enPassant)
	halfClock <- row.names(position)[game]
	halfClock <- rev(!grepl("[KQRNB]",halfClock) | grepl("x",halfClock))
	halfClock <- min(which(halfClock))
	string <- paste(string,halfClock)
	fullMove <- unlist(strsplit(row.names(position[game_pgn,]),"\\.|_"))
	fullMove <- fullMove[min(grep("^\\d+$",fullMove))]
	string <- paste(string, fullMove)
	string
}
