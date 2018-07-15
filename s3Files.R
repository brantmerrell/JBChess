# A function identifying the ID range of "S3://jbchess/data/chess.com IDs <range>.csv" files

s3Files <- function(File="s3Files.txt"){
	Files <- readLines(File)
	for(n in 1:3){
		Files <- sub(" +", ";", Files)
	}
	Files <- matrix(unlist(strsplit(Files,";")), ncol=4,byrow=T)
	Files <- data.frame(Files, stringsAsFactors=F)
	colnames(Files) <- c("Date", "Time", "Size", "File")
	Files$Size <- as.numeric(Files$Size)
	IDs <- gsub("[[:alpha:]]|[ \\./:]","",Files$File)
	IDs[!grepl("chess.com IDs", Files$File)] <- ""
	Files[,"startID"] <- as.numeric(gsub("-\\d+","",IDs))
	Files[,"stopID"] <- as.numeric(gsub("^\\d+-","",IDs))
	Files <- Files[order(Files$startID),]
	row.names(Files) <- 1:nrow(Files)
	return(Files)
}

