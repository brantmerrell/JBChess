validate_fen <- function(fen, fiftyMoveRule = T) {
    values <- unclass(fen)
    if(!all(!is.na(values))) {
        stop('NA values not allowed in FEN')
    }
    if(attr(fen, 'class') != 'fen') {
        stop(paste('class of FEN object must be fen. Actual:', attr(fen, 'class')))
    }
    if(!attr(fen, 'activeColor') %in% c('w','b')) {
        stop(paste('activeColor of FEN object must be w or b. Actual:', attr(fen, 'activeColor')))
    }
    castleOptions <- strsplit(attr(fen, 'castling'), '')[[1]]
    if(any(!castleOptions %in% c('K', 'Q', 'k', 'q'))) {
        stop(paste('castling string attribute of FEN object should only include characters K, k, Q, q. Actual:', attr(fen, 'castling')))
    }
    if(sum(duplicated(castleOptions)) > 0) {
        stop(paste('castling string attribute of FEN objects should contain no duplicate characters. Actual:', attr(fen, 'castling')))
    }
    enPassantPattern <- '-|[abcdefgh][1234567]'
    if(!grepl(pattern = enPassantPattern, x = attr(fen, 'enPassant'))) {
        stop(
            paste(
                'enPassant string attribute of FEN objects should match pattern', 
                enPassantPattern, 
                '; Actual: ', 
                attr(fen, 'enPassant')
            )
        )
    }
    if(attr(fen, 'halfMove') < 0) {
        stop('halfMove clock must be positive')
    }
    if(attr(fen, 'halfMove') > 50 & fiftyMoveRule) {
        stop('halfMove clock exceeds fiftyMoveRule')
    }
    if(attr(fen, 'fullMove') < 1) {
        stop('fullMove clock cannot be less than 1')
    }
    return(fen)
}