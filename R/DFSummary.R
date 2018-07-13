DFSummary <- function(DF){
  Class <- NULL
  for(m in 1:ncol(DF)){Class <- c(Class, class(DF[,m])[1])}
  DFSum <- data.frame(variable = colnames(DF),
                      class = Class,
                      unique = unlist(lapply(lapply(DF, unique),length)),
                      is.na = unlist(lapply(lapply(DF,is.na),sum)),
                      has.content = unlist(lapply(lapply(DF, 
                                                         grepl, 
                                                         pattern = "."), 
                                                  sum)),
                      row.names = 1:ncol(DF),
                      stringsAsFactors = F)
  DFSum$empty.strings <- nrow(DF) - DFSum$is.na - DFSum$has.content
  DFSum
}