board <- function (position_vec) {
  if(class(position_vec)=="data.frame"){
    position_vec <- unlist(position_vec[nrow(position_vec),])
  }
	DF <-matrix(position_vec, nrow = 8, ncol = 8, 
	                 dimnames = list(8:1, letters[1:8]))
	DF
}
