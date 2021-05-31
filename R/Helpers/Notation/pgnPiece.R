pgnPiece <- function(pgn){
  test <- unlist(lapply(chesspatterns, grepl, x = pgn))
  test <- names(test)[test]
	test <- test[test %in% c("queen","king", "knight", "rook", "bishop", "pawn")]
	if(length(test)>1){stop("pgn tests positive for multiple pieces")}
	if(length(test)<1){stop("pgn test negative for all pieces")}
	test
}
