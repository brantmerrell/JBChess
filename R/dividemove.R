dividemove <- function(n=1,incolor=c("white","black")){
  chesspgn <- "C:/Users/Josh/Documents/chess/chesspgn.csv"
  chesspgn <- read.csv(chesspgn,colClasses="character")
  while(sum(chesspgn$move %in% n)<500 & max(n)<max(as.numeric(chesspgn$move))){
    n <- c(n,(max(n)+1))
  }
  workframe <- subset(chesspgn,chesspgn$color %in% incolor & chesspgn$move %in% n)
  if(nchar(min(n))==1){filepath <- paste("C:/Users/Josh/Documents/Chess/move_0")}
  if(nchar(min(n))==2){filepath <- paste("C:/Users/Josh/Documents/Chess/move_")}
  if(2<nchar(min(n))){
    filepath <- paste("C:/Users/Josh/Documents/Chess/move_0")
    message("I might have to adjust the filenames for this length of game")
  }
  filepath <- paste(filepath,min(n),".csv",sep="")
  if(500<=nrow(workframe)){
    write.csv(workframe,filepath,row.names=FALSE)
    print(filepath)
    sampleframe <- rbind(workframe[1,],
                       workframe[as.integer(nrow(workframe)*.2),],
                       workframe[as.integer(nrow(workframe)*.4),],
                       workframe[as.integer(nrow(workframe)*.6),],
                       workframe[as.integer(nrow(workframe)*.8),],
                       workframe[as.integer(nrow(workframe)),])
  }
  else{
    sampleframe <- tail(workframe,3)
  }
  sumframe <- data.frame(King=sum(workframe$piece=="King"),
                       Queen=sum(workframe$piece=="Queen"),
                       Rook=sum(workframe$piece=="Rook"),
                       Knight=sum(workframe$piece=="Knight"),
                       Bishop=sum(workframe$piece=="Bishop"),
                       pawn=sum(workframe$piece=="pawn"),
                       total=nrow(workframe))
  print(sampleframe)
  print(sumframe)
}
