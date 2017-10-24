arguments <- commandArgs(T)

invisible(lapply(list.files("R","(scrape|regex|weave).*\\.R$", full.names=T), source))

arguments <- strsplit(arguments,"[[:punct:]]")[[1]]
arguments <- as.numeric(arguments)

arguments <- seq(arguments[1], arguments[length(arguments)])

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

filename <- paste("data/chess.com IDs ", min(arguments),"-",  max(arguments),".csv", sep="")

write.csv(DF, filename, row.names=F)

print(filename)


