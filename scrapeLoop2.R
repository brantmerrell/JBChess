arguments <- commandArgs(T)

invisible(lapply(list.files("R","(scrape|regex|weave).*\\.R$", full.names=T), source))

arguments <- strsplit(arguments,"[[:punct:]]")[[1]]
arguments <- as.numeric(arguments)

arguments <- seq(min(arguments), max(arguments))

filePattern <- paste(min(arguments), "-", max(arguments), ".csv", sep="")

filename <- paste("data/chess.com IDs", filePattern)

Files <- readLines("temp.txt")

if(sum(grepl(filename,Files))==0){

	DF <- scrape.chess.com(arguments[1])

	print(DF$ID)

	for(n in arguments[-1]){
		DF <- weave.rbind(DF,scrape.chess.com(n))
		print(tail(DF$ID,3))
	}

	TEST <- grepl("thinkboolean", DF$White)

	TEST <- TEST | grepl("thinkboolean", DF$Black)

	if(0<sum(TEST)){
		DF1 <- DF[TEST,]
		print(tail(DF1))
		if(file.exists("../thinkboolean.csv")){
			DF1 <- weave.rbind(DF1, read.csv("../thinkboolean.csv"))
		}
	write.csv(DF1, "../thinkboolean.csv", row.names=F)
	}

write.csv(DF, filename, row.names=F)

print(filename)
}

