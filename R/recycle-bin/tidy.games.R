tidy.games <- function(pgnFile){
		pgnLines <- readLines(pgnFile)
		notationLines <- 
			(grep("\\d\\.[KQRNB]?[abcdefgh12345678]?x?[abcdefgh][1234567]",pgnLines))
		range <- data.frame(start=grep("\\[Event",pgnLines),stringsAsFactors = F)
		range <- cbind(range,"stop"=c(range[-1,'start']-1,length(pgnLines)), stringsAsFactors=F)
		for(n in 1:nrow(range)){
			m <- seq(range[n,'start'],range[n,'stop'])
			meta <- pgnLines[m][grep("\\[",pgnLines[m])]
			pgn <- paste(pgnLines[m][!pgnLines[m] %in% meta], collapse = " ")
			pgn <- gsub(" {2,}"," ",pgn)
			meta <- unlist(strsplit(meta,"\""))
			gsub("\\[| $","",meta[grep("^\\[",meta)])
			DF <- data.frame(matrix(meta[-grep("\\[|\\]",meta)],nrow=1), stringsAsFactors = F)
			colnames(DF) <- gsub("\\[| $","",meta[grep("^\\[",meta)])
			DF <- cbind(DF,pgn=pgn, stringsAsFactors=F)
			if(n==1){metaChess <- DF}else{
				DF <- DF[,colnames(DF) %in% colnames(metaChess)]
				metaChess <- rbind(metaChess,DF)
			}
		}
		metaChess
	}
