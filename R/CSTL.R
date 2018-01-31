CSTL <- function(X){
	if(grepl("O-O",X)){
		kingColumn <- "g"
		rookColumn <- "f"
		rookVacant <- "h"
	}
	if(grepl("O-O-O",X)){
		kingColumn <- "c"
		rookColumn <- "d"
		rookVacant <- "a"
	}
	if(grepl(whitePattern,X)){
		Row <- 1
	}
	if(grepl(blackPattern,X)){
		Row <- 8
	}
	list(king=paste(kingColumn,Row,sep=""),
			 rook=paste(rookColumn,Row,sep=""),
			 kingVacant=paste("e",Row,sep=""),
			 rookVacant=paste(rookVacant,Row,sep=""))
}
