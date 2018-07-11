chesspatterns <- list(
  bishop = "[Bb][Ii][Ss][Hh][Oo][Pp]|(^\\d+\\.+Bx?([[:lower:]]\\d)+$)",
  black = "[Bb][Ll][Aa][Cc][Kk]|\\.{3}",
  capture = "x\\[[:lower:]]\\d",
  castle = "[Cc][Aa][Ss][Tt][Ll][Ee]|O-O",
  castleQ = "[Cc][Aa][Ss][Tt][Ll][Ee]|O-O-O",
  checkmate = "#",
  check = "\\+",
  king = "[Kk][Ii][Nn][Gg]|(^\\d+\\.+Kx?([[:lower:]]\\d)+$)",
  knight = "[Kk][Nn][Ii][Gg][Hh][Tt]|(\\d+\\.+N([[:lower:]]|\\d)?x?[[:lower:]]\\d(#|\\+)?$)",
  pawn = "[Pp][Aa][Ww][Nn]|(\\d+\\.+[[:lower:]]?x?([[:lower:]]\\d)+$)",
  pgn = "\\d\\..*[abcdefgh].*[12345678]",
  queen = "[Qq][Uu][Ee][Ee][Nn]|(^\\d+\\.+Qx?([[:lower:]]\\d)+[[:punct:]]+?$)",
  rook = "[Rr][Oo][Oo][Kk]|(^\\d+\\.+Rx?([[:lower:]]\\d)+$)",
  white = "[Ww][Hh][Ii][Tt][Ee]|([[:alnum:]]\\.{1,1}[[:alpha:]])"
)
