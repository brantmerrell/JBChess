arguments <- commandArgs(T)

invisible(lapply(list.files("R","(scrape|regex|weave).*\\.R$", full.names=T), source))

arguments <- strsplit(arguments,"[[:punct:]]")[[1]]
arguments <- as.numeric(arguments)

arguments <- seq(arguments[1], arguments[length(arguments)])

DF <- scrape.live(id=arguments[1])

print(DF[,grepl("white|black|id",colnames(DF),ignore.case=T)])

for(n in arguments[-1]){
	DF <- weave.rbind(DF,scrape.live(id=n))
	print(DF[grepl("white|black|id",colnames(DF),ignore.case=T)])
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

filename <- paste("data/chess.com LIVE IDs ", min(arguments),"-",  max(arguments),".csv", sep="")

write.csv(DF, filename, row.names=F)

print(filename)


