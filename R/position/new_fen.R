new_fen <- function(
    placement = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR',
    activeColor = 'w',
    castling = 'KQkq',
    enPassant = '-',
    halfMove = as.integer(0),
    fullMove = as.integer(1)
) {
    stopifnot(is.character(placement))
    stopifnot(activeColor %in% c('w', 'b'))
    stopifnot(is.character(castling))
    stopifnot(is.character(enPassant))
    stopifnot(is.integer(halfMove))
    stopifnot(is.integer(fullMove))
    fen <- paste(placement, activeColor, castling, enPassant, halfMove, fullMove)
    structure(
        fen, 
        class="fen", 
        activeColor = activeColor,
        castling = castling,
        enPassant = '-',
        halfMove = halfMove,
        fullMove = fullMove
    )
}