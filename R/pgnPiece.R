pgnPiece <- function(pgn){
  test <- function(L){grepl(L,pgn)}
  P <- c("K","Q","R","N","B")
  Piece=c("King","Queen","Rook","Knight","Bishop")
  df <- data.frame(P=P, Piece=Piece,assess=unlist(lapply(P,test)))
  if(sum(df$assess)==0){
    return("pawn")
  }
  if(sum(df$assess)==1){
    return(as.character(subset(df$Piece,df$assess==TRUE)))
  }
  if(1<sum(df$assess)){
    stop("Input contains extra piece")
  }
}
