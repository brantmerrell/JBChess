pathPrior <- function(piece,square){
	Col <- which(letters==strsplit(square,"")[[1]][1]) # identify the column
	Row <- as.numeric(strsplit(square,"")[[1]][2]) # identify the row
	
	# if the piece is a pawn:
	if(grepl(chesspatterns$pawn,piece)){
		
		# if the pawn's color has not yet been defined, define it as nothing. Why?
		if(!exists("color")){color <- ""}
		
		# if the pawn is black, it moves down the board:
		if(grepl(chesspatterns$black,piece)){direction <- -1} 
		
		# if the pawn is white, it moves up the board:
		if(grepl(chesspatterns$white,piece)){direction <- 1} 
		
		# create function generating attack columns:
		PRange <- function(center){ 
			
			# attack columns are the current column plus & minus 1...
			P <- c((center-1),(center+1)) 
			
			# ... minus any columns outside board boundaries
			P[0<P & P<=8] 
		}
		
		# generate pawn attack squares:
		AttSq <- paste(letters[PRange(Col)],Row+direction,sep="")
		
		# determine whether pawn has option to move two spaces:
		ifelse((direction==-1 & Row==7)|(direction==1 & Row==2),distance <- 1:2,distance <- 1)
		
		# generate squares to which the pawn can move without attacking:
		Path <- paste(letters[Col],Row+(distance*direction),sep="") 
		
		# return all squares to which the pawn can move:
		Path <- c(Path,AttSq)
	}
	if(grepl(chesspatterns$bishop,piece)){ # check whether the piece is a bishop
		B <- function(x){ # define a function that takes as input any row, 
			V <- c(x+Col-Row,Col+Row-x) # already knows bishop location, 
			paste(letters[x],V[0<V & V<=8],sep="") 
			# and returns squares on that row where the bishop can move
		}
		Path <- unique(unlist(sapply(1:8,B))) # input all rows on the board
	}
	if(grepl(chesspatterns$knight,piece)){ # check whether the piece is a knight
		NRange <- function(center,dist){ # create 1-dim function for plus & minus board distance
			dist <- unique(c(dist,dist*-1)) # distance = plus and minus displacement
			N <- expand.grid(center,dist) # combinations of center and displacements
			N <- apply(N,1,sum) # add center and displacements for 1-d location
			N[0<N & N<=8] # return only what exists within board boundaries
		}
		Path <- rbind(expand.grid(letters[NRange(Col,1)],NRange(Row,2)), # generate 1xCol and 2xRow
								expand.grid(letters[NRange(Col,2)],NRange(Row,1))) # generate 2xCol and 1xRow
		Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
	}
	if(grepl(chesspatterns$rook,piece)){ # check whether the piece is a rook
		Path <- unique(c(paste(letters[Col],1:8,sep=""), # generate squares along column
									 paste(letters[1:8],Row,sep=""))) # generate squares along row
	}
	if(grepl(chesspatterns$queen,piece)){ # check whether the piece is a Queen
		# replicate bishop function:
		B <- function(x){ 
			V <- c(x+Col-Row,Col+Row-x) 
			paste(letters[x],V[0<V & V<=8],sep="")
		}
		Path <- c(unique(unlist(sapply(1:8,B))), # generate diagonals
						unique(c(paste(letters[Col],1:8,sep=""), # generate squares along column
										 paste(letters[1:8],Row,sep="")))) # generate squares along row
	}
	if(grepl(chesspatterns$king,piece)){ # check whether the piece is a King
		KRange <- function(center){ # create 1-dim function for plus to minus board distance (1) 
			K <- (center-1):(center+1) # broaden center in two directions
			K[0<K & K<=8] # return only results within board boundaries
		}
		Path <- expand.grid(letters[KRange(Col)], # generate all combinations of King's column range
											KRange(Row)) # generate all combinations of King's row range
		Path <- paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
	}
	Path[Path!=square] # a piece can't move to the square on which it already resides!
}
