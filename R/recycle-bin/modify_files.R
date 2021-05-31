ARGS <- commandArgs()
ARGS <- ARGS[which(ARGS=="--args")+(1:length(ARGS))]
DATA <- read.csv(ARGS[1], stringsAsFactors=F)
write.csv(unique(DATA), ARGS[1], row.names=F)

source("R/DFSummary.R")
summ <- DFSummary(DATA)
summ$File <- ARGS[1]
if(file.exists("data/data_summaries.csv")){
    summ <- rbind(read.csv("data/data_summaries.csv", stringsAsFactors=F),
                  summ)
}
write.csv(summ,"data/data_summaries.csv", row.names = F)
