chesscolor <- function(pgn){
  if(ncol(read.table(textConnection(pgn),sep="."))==4){
    return("black")
  }
  if(ncol(read.table(textConnection(pgn),sep="."))==2){
    return("white")
  }
  if(!(ncol(read.table(textConnection(pgn),sep=".")) %in% c(2,4))){
    stop("invalid number of dots")
  }
}
