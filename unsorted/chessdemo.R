

dir.exists("chess")
if(!dir.exists("chessdoodles/positions")){dir.create("chessdoodles/positions")}
head(colnames(position));tail(colnames(position))
write.csv(position,"chessdoodles/positions/initial.csv",row.names=T)

# have a function to count all the pieces and their squares on a given row of the position frame:
# pieceCount(game_pgn){...data.frame(square,piece)}

# a function to print the chessboard as a data frame:
# board<-function(game_pgn=2){...data.frame(a,b,c,d,e,f,g,h)

# A function is in order to tell us exactly where one piece can move. 
#   First it must be able to identify each piece, color, and type of move.

#   pieces:
    pawnPattern<-"[Pp][Aa][Ww][Nn]|(\\d+\\.+[[:lower:]]?x?([[:lower:]]\\d)+$)"
        grepl(pawnPattern,"1...e5") # [1] TRUE
        grepl(pawnPattern,"5...xf7") # [1] TRUE
    knightPattern<-"[Kk][Nn][Ii][Gg][Hh][Tt]|(\\d+\\.+N([[:lower:]]|\\d)?x?[[:lower:]]\\d(#|\\+)?$)"
        grepl(knightPattern,"1...Ne5") # [1] TRUE
        grepl(knightPattern,"1...e5") # [1] FALSE
        grepl(knightPattern,"1...Nxe5") # [1] TRUE
    bishopPattern<-"[Bb][Ii][Ss][Hh][Oo][Pp]|(^\\d+\\.+Bx?([[:lower:]]\\d)+$)"
        grepl(bishopPattern,"1...Be5") # [1] TRUE
    rookPattern<-"[Rr][Oo][Oo][Kk]|(^\\d+\\.+Rx?([[:lower:]]\\d)+$)"
        grepl(rookPattern,"1...Re5") # [1] TRUE
    queenPattern<-"[Qq][Uu][Ee][Ee][Nn]|(^\\d+\\.+Qx?([[:lower:]]\\d)+[[:punct:]]+?$)"
        grepl(queenPattern,"1...Qxe5") # [1] TRUE
    kingPattern<-"[Kk][Ii][Nn][Gg]|(^\\d+\\.+Kx?([[:lower:]]\\d)+$)"
        grepl(kingPattern,"1...Kxe5") # [1] TRUE
    piecePatterns <- c(King = kingPattern, Queen = queenPattern, Rook = rookPattern,
                       Knight = knightPattern, Bishop = bishopPattern, pawn = pawnPattern,
                       Castle = castlePattern)
        
#   colors:
    blackPattern<-"[Bb][Ll][Aa][Cc][Kk]|\\.{3}"
    whitePattern<-"[Ww][Hh][Ii][Tt][Ee]|([[:alnum:]]\\.{1,1}[[:alpha:]])"
    colorPatterns <- c(white = whitePattern, black = blackPattern)

#   types:
    castlePattern<-"[Cc][Aa][Ss][Tt][Ll][Ee]|O-O"
    castleQPattern<-"O-O-O"
    capturePattern<-"x\\[[:lower:]]\\d"
    checkPattern<-"+"
    checkmatePattern<-"#"

#   detect pgn
    pgnPattern<-"\\d\\."

# A piece's mobility is limited by the circumstances of the board, 
# but pathPrior() generates a list of squares for any piece on an empty board.
# pathPrior<-function(piece,square){...vector of squares}

# They also need to know if a square is vacant, occupied by white, or occupied by black
#   squareColor<-function(square,game_pgn){...blackPattern OR whitePattern OR NA)
    squareColor("a5",2)
    squareColor("a2",2)
    squareColor("a7",2)

# 
# pawnPost.<-function(square,game_pgn="000_zero){
#   define "forward" and "backward" based on color of pawn
#   show adjacent-diagonal-frontal squares only when an opposite color resides in them
#   show two squares ahead only if pawn has not moved and is not blocked
#   show one square ahead only if pawn is not blocked by either color
# ...return vector of squares}

# no pawn is on e5, e1, or e8:

pawnPost.("e5",game_pgn="000_zero") # [1] "e6" "e4"
# e5 is occupied by neither color, so e5 pawn is neither color, which is impossible.
# Function assumes this impossible pawn can move both directions.

pawnPost.("e1", game_pgn="000_zero") # [1] NA
# e1 is occupied by the white king. pawnPost recognizes e1 as white, but assumes the 
# occupant is a pawn. A pawn in the king's place at this position would be unable to move.

pawnPost.("e7", game_pgn="000_zero") # [1] "e6" "e5"

# knightPost.<-function(square,game_pgn){
#   define knight color
#   generate squares where the knight can move 
#   subtract off-board squares and squares occupied by knight's color
}

knightPost.("e7", game_pgn="000_zero") # [1] "f5" "d5" "g6" "c6"
# e7 is occupied by a black pawn. knightPost. pretends it is a black knight.

knightPost.("e2", game_pgn="000_zero") # [1] "f4" "d4" "g3" "c3"
# e2 is occupied by a white pawn. knightPost. pretends it is a white knight.

# bishopPost.<-function(square,game_pgn){
#   determine bishop color,
#   generate a list of diagonal squares to which the bishop could move on an empty board,
#   create Test(square){...TRUE OR FALSE} to return FALSE if:
#       1. betweens() shows that there are pieces between start and destination squares, or
#       2. a piece of the same color resides on destination square
#   use Test() to filter out squares to which bishop cannot move,
#   return vector of squares to which bishop can move
#}

bishopPost.("a2",game_pgn="000_zero") # [1] "b3" "c4" "d5" "e6" "f7"
# a2 is occupied by a white pawn, bishopPost pretends it is a white bishop

bishopPost.("c1",game_pgn="000_zero") # character(0)
# c1 is occupied by a white bishop, and is trapped between other white pieces

bishopPost.("c3",game_pgn="000_zero") # 
# c3 is not occupied by either color, function assumes c3 bishop can capture either color

# rookPost.<-function(square,game_pgn){...vector of squares}




# position<-position[1:2,]



# file.remove(list.files("chess","97722598.R",full.names=T)[1])
