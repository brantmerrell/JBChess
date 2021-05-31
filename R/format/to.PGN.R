to.PGN <- function(string, move=0, castle=NULL, check=NULL, enpassant=NULL, position_vec=NULL){
  string <- gsub("white",".", string, ignore.case = T)
  string <- gsub("black","...", string, ignore.case = T)
  string <- gsub("King","K", string, ignore.case = T)
  string <- gsub("Queen","Q", string, ignore.case = T)
  string <- gsub("Rook","R", string, ignore.case = T)
  string <- gsub("Bishop","B", string, ignore.case = T)
  string <- gsub("Knight","N", string, ignore.case = T)
  string <- gsub("pawn| ","", string, ignore.case = T)
  string <- paste0(move, string)
}