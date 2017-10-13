completeID <- function(ID){
  sample1 <- sort(as.numeric(subset(chesspgn$move,chesspgn$ID==ID & chesspgn$color=="white")))
  test1 <- ifelse((length(sample1)!=0),identical(paste(sample1),paste(1:max(sample1))),FALSE)
  sample2 <- sort(as.numeric(subset(chesspgn$move,chesspgn$ID==ID & chesspgn$color=="black")))
  test2 <- ifelse((length(sample2)!=0),identical(paste(sample2),paste(1:max(sample2))),FALSE)
  ifelse((test1==TRUE & test2==TRUE),return("complete"),return("incomplete"))
}
