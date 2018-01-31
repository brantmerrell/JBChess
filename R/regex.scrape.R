regex.scrape <- function(Link, type, errorMessage="unfound"){
		Html <- tryCatch(readLines(Link, warn = F),error=function(e){return(errorMessage)})
		
		STR <- Html[grepl("vs",Html)]
		STR <- unlist(strsplit(STR, "\""))
		STR <- STR[grepl("vs",STR)]
		STR <- STR[grepl("(\\(\\d+\\).+){2}", STR)]
		STR <- unique(STR)
		if(length(STR)==0){STR <- "0"}
		if(STR=="0" & type %in% c("correspondence","live")){
			return(data.frame(White="",Black="",stringsAsFactors = F))
		}else{
			if(type == "live"){
				Time <- Html[grep("Time:$",Html)+1]
			}
			if(type=="correspondence"){
				Time <- unlist(strsplit(STR,"[[:punct:]]"))
				Time <- Time[grepl("over",Time)]
				Time <- strsplit(Time,"over ")[[1]][2]
			}
			if(grepl("live|corres",type)){
				White <- strsplit(STR," vs.? ")[[1]][1]
				WhiteElo <- unlist(strsplit(White, "[[:punct:]]"))
				WhiteElo <- gsub(" ","", WhiteElo[grepl("^\\d+$",WhiteElo)])
				if(length(WhiteElo)==0){WhiteElo <- 0}
				White <- strsplit(White, "[[:blank:]]")[[1]][1]
				
				Black <- strsplit(STR," vs |\\.")[[1]][2]
				BlackElo <- unlist(strsplit(Black,"[[:punct:]]"))
				BlackElo <- gsub(" ", "", BlackElo[grepl("^\\d+$", BlackElo)])
				Black <- unlist(strsplit(Black,"[[:blank:]]"))
				if(length(BlackElo)==0){BlackElo <- 0}
				Black <- Black[grepl(".",Black)][1]
				
				winner <- unlist(strsplit(STR,"[[:punct:]]"))
				winner <- winner[grepl("won",winner)]
				if(length(winner)==0){winner=""}else{
					winner <- gsub(" ","",strsplit(winner,"won")[[1]][1])
				}
				
				if(winner==White){Result <- "1-0"}
				if(winner==Black){Result <- "0-1"}
				if(grepl("draw",STR)){Result <- "draw"}
				if(grepl("stalemate",STR)){Result <- "stalemate"}
				if(!exists("Result")){Result <- ""}
				
				if(grepl("draw", STR)){Termination <- "draw"}
				if(grepl("won on time",STR)){Termination <- "time"}
				if(grepl("won by checkmate", STR)){Termination <- "checkmate"}
				if(grepl("won by resignation", STR)){Termination <- "resignation"}
				if(grepl("game abandoned", STR)){Termination <- "game abandoned"}
				if(!grepl("draw|on time|by checkmate|by resignation|game abandoned", STR)){
					stop("new termination test required")
					print(STR)
				}
				
				Moves <- unlist(strsplit(STR,"in|by|over|\\."))
				Moves <- Moves[grepl("\\d moves",Moves)][1]
				Moves <- gsub("[[:alpha:]]|[[:blank:]]","",Moves)
				
				return(data.frame(White,Black, Result, WhiteElo, BlackElo, Time, Moves, Termination,
													stringsAsFactors = F))
			}
		}
		if(type=="open"){
				
			# game notation matches the format n.rankOrFile?_piece?_capture?_square
				pgnPattern <- "\\d\\.[[:blank:]]?([[:alnum:]])?[KQRNB]?x?[abcdefgh]\\d"
				pgn <- Html[grep(pgnPattern,Html)]
				if(length(pgn)!=0){
					pgn <- paste(pgn[grep("^1",pgn)[1]:length(pgn)], collapse = " ")
					pgn <- gsub("\\.[[:blank:]]",".",pgn)
				}else{pgn <- ""}
					
				info <- Html[grep("^\\[.+&quot.+\\]$",Html)]
				info <- gsub("^\\[|&quot;]$","",info)
				info <- unlist(strsplit(info," &quot"))
				if(length(info) %% 2 != 0){info <- info[-length(info)]}
				if(2<length(info)){
		openDF <- info[seq(2,length(info),2)]
		openDF <- gsub(";", "", openDF)
		openDF <- data.frame(matrix(openDF, nrow=1), stringsAsFactors=F)
		colnames(openDF) <- info[seq(1,length(info),2)]
		openDF <- cbind(openDF,pgn,stringsAsFactors=F)
				}else{openDF <- data.frame(White="",Black="", stringsAsFactors = F)}
	return(openDF)
			}
	}
