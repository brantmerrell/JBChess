piecePost. <- function(square,game_pgn){
	piece <- as.vector(position[game_pgn,square])
	if(grepl(chesspatterns$pawn,piece)){return(pawnPost.(square,game_pgn))}
	if(grepl(chesspatterns$knight,piece)){return(paste("",knightPost.(square,game_pgn),sep=""))}
	if(grepl(chesspatterns$bishop,piece)){return(paste("",bishopPost.(square,game_pgn),sep=""))}
	if(grepl(chesspatterns$rook,piece)){return(paste("",rookPost.(square,game_pgn),sep=""))}
	if(grepl(chesspatterns$queen,piece)){return(paste("",queenPost.(square,game_pgn),sep=""))}
	if(grepl(chesspatterns$king,piece)){return(paste("",kingPost.(square,game_pgn),sep=""))}
}
