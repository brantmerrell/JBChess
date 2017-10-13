empty <- function(gameName = "000"){
  # to create board, first list each combination of columns a:h & rows 1:8:
  board <- expand.grid(columns=letters[1:8],rows=1:8)
  
  # arrange columns and rows such that they will read left-to-right and bottom-to-top:
  board <- board[order((board$columns), 
                     rev(board$rows)),] 
  # - this can feel counter-intuitive in <r>, as data frames typically begin at the top-left
  
  # convert data frame to vector:
  board <- paste(board[,1],board[,2],sep="")
  
  # since this "board" is now linear, it's essentially a list of squares:
  squares <- board
  
  # Every position of every piece must be available for every portion of every game.
  # It could be awkward storing a separate data frame per move per game, 
  # so it's useful to have a 64-column data frame with an indefinite number of rows,
  # particularly if rows can be accessed by a game_pgn combination:
  
  position <- data.frame(matrix(NA,nrow=1,ncol=64))
  row.names(position) <- paste(gameName,"empty", sep = "_")
  # - "empty" refers to an empty chessboard, "zero" is a set-up chessboard with no moves made
  
  # the column names for the position frame should be the list of squares:
  colnames(position) <- squares
  position
}

