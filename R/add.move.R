add.move <- function(ID,pgn){
  if(nchar(ID)==10){type <- "livechess"}
  if(nchar(ID)==9){type <- "echess"}
  if(!(pgn %in% subset(chesspgn$pgn,chesspgn$ID==ID))){
    filepath <- "C:/Users/Josh/Documents/chess/chesspgn.csv"
    chesspgn <- read.csv(filepath,colClasses="character")
    color <- properties.pgn(pgn)$color
    move <- properties.pgn(pgn)$move
    piece <- properties.pgn(pgn)$piece
    xcoor <- properties.pgn(pgn)$x
    ycoor <- properties.pgn(pgn)$y
    sq <- properties.pgn(pgn)$sq
    newrow <- c(type,ID,pgn,color,move,piece,xcoor,ycoor,sq)
    chesspgn <- rbind(chesspgn,newrow)
    write.csv(unique(chesspgn),
              filepath,
              row.names=FALSE)
  }
  tail(read.csv(filepath),2)
}
