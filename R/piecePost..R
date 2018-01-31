piecePost. <- function(square,game_pgn){
	piece <- as.vector(position[game_pgn,square])
	if(grepl(pawnPattern,piece)){return(pawnPost.(square,game_pgn))}
	if(grepl(knightPattern,piece)){return(paste("",knightPost.(square,game_pgn),sep=""))}
	if(grepl(bishopPattern,piece)){return(paste("",bishopPost.(square,game_pgn),sep=""))}
	if(grepl(rookPattern,piece)){return(paste("",rookPost.(square,game_pgn),sep=""))}
	if(grepl(queenPattern,piece)){return(paste("",queenPost.(square,game_pgn),sep=""))}
	if(grepl(kingPattern,piece)){return(paste("",kingPost.(square,game_pgn),sep=""))}
}
