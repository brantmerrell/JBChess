mobility_board <- function(position_vec, patterns = chesspatterns[c("white","black")]){
  if(is.null(names(patterns))) names(patterns) <- patterns
  if(class(position_vec)=="data.frame"){
    position_vec <- unlist(position_vec[nrow(position_vec),])
  }
  DF <- pieceCount(position_vec)
	for (n in 1:nrow(DF)) {
		options1 <- mobility_piece(DF[n,"square"], position_vec)
		DF[n, "count"] <- length(options1)
	}
  result <- c(total = sum(DF$count))
  for(n in 1:length(patterns)){
    result <- c(result, sum(DF[grepl(patterns[n],DF$piece, ignore.case=T),"count"]))
    names(result)[length(result)] <- names(patterns)[n]
  }; rm(n)
  return(result)
}
