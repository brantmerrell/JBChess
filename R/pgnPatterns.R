bishopPattern <- "[Bb][Ii][Ss][Hh][Oo][Pp]|(^\\d+\\.+Bx?([[:lower:]]\\d)+$)"
blackPattern <- "[Bb][Ll][Aa][Cc][Kk]|\\.{3}"
capturePattern <- "x\\[[:lower:]]\\d"
castlePattern <- "[Cc][Aa][Ss][Tt][Ll][Ee]|O-O"
castleQPattern <- "[Cc][Aa][Ss][Tt][Ll][Ee]|O-O-O"
checkmatePattern <- "#"
checkPattern <- "+"
kingPattern <- "[Kk][Ii][Nn][Gg]|(^\\d+\\.+Kx?([[:lower:]]\\d)+$)"
knightPattern <- "[Kk][Nn][Ii][Gg][Hh][Tt]|(\\d+\\.+N([[:lower:]]|\\d)?x?[[:lower:]]\\d(#|\\+)?$)"
pawnPattern <- "[Pp][Aa][Ww][Nn]|(\\d+\\.+[[:lower:]]?x?([[:lower:]]\\d)+$)"
pgnPattern <- "\\d\\..*[abcdefgh].*[12345678]"
queenPattern <- "[Qq][Uu][Ee][Ee][Nn]|(^\\d+\\.+Qx?([[:lower:]]\\d)+[[:punct:]]+?$)"
rookPattern <- "[Rr][Oo][Oo][Kk]|(^\\d+\\.+Rx?([[:lower:]]\\d)+$)"
piecePatterns <- c(King=kingPattern, Queen=queenPattern, Rook=rookPattern, 
									 Knight=knightPattern, Bishop=bishopPattern, pawn=pawnPattern)
whitePattern <- "[Ww][Hh][Ii][Tt][Ee]|([[:alnum:]]\\.{1,1}[[:alpha:]])"
colorPatterns <- c(white=whitePattern, black=blackPattern)
