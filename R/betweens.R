betweens <- function(square1,square2,game_pgn){
  Col1 <- which(letters==strsplit(square1,"")[[1]][1]) # convert square's column letter to a number
  Row1 <- as.numeric(strsplit(square1,"")[[1]][2]) # extract square's row number
  Col2 <- which(letters==strsplit(square2,"")[[1]][1]) # convert square2 column letter to a number
  Row2 <- as.numeric(strsplit(square2,"")[[1]][2]) # extract square2 row number
  if(length(Col1:Col2)!=length(Row1:Row2) & # check whether squares do NOT share a diagonal
       !(0 %in% c(Col1-Col2,Row1-Row2))){  # check whether squares do NOT share a line
    # if none of these, something is wrong. Send message:
    stop("these two squares are not in the same horizontal, vertical, or diagnal")
  }
  btwns <- paste(letters[Col1:Col2],Row1:Row2,sep="") #list squares between the squares
  return(data.frame(squares=btwns,
                    occupants=as.character(position[game_pgn,btwns]))) #retrieve occupants
}
