arguments <- commandArgs(T)

invisible(lapply(list.files("R","(scrape|regex|weave).*\\.R$", full.names=T), source))

arguments <- strsplit(arguments,"[[:punct:]]")[[1]]
arguments <- as.numeric(arguments)

arguments <- seq(arguments[1], arguments[length(arguments)])

DF <- scrape.chess.com(arguments[1])

for(n in arguments[-1]){
	DF <- rbind(DF,scrape.chess.com(n))
}

filename <- paste("data/", min(arguments),"-",  max(arguments),".csv", sep="")

write.csv(DF, filename, row.names=F)
