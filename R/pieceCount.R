pieceCount <- function(game_pgn=nrow(position)){
  
  # identify which part of the board is occupied
  Col <- !apply(position[game_pgn,],2,is.na)
  
  # identify the pieces on the occupied portion of the
  
  if(!sum(Col)==1){
    square <- colnames(position)[Col]
    piece <- as.character(as.matrix(position[game_pgn,Col]))
    DF <- data.frame(square=colnames(position[game_pgn,Col]),
                     piece=as.character(as.matrix(position[game_pgn,Col])),
                     stringsAsFactors = F)
  }else{
    DF <- data.frame(square=colnames(position[game_pgn,Col]),
                     piece=piece, 
                     stringsAsFactors = F)
  }
  DF
}
