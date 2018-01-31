queenPost. <- function(square,game_pgn){
	R <- rookPost.(square,game_pgn)
	B <- bishopPost.(square,game_pgn)
	sort(c(R,B))
}
