# _Post.() functions internally generate where a piece could move on an empty board, 
# but they use the board's setup to modify the results.
# They obtain the setup from the input 'game_pgn', 
# which references a row on the data frame 'position.'
# If the 

pawnPost.<-function(square,game_pgn="000_zero"){
  if(!grepl(pawnPattern,position[game_pgn,square])){stop("this piece is not a pawn")}
  if(grepl(blackPattern,position[game_pgn,square])){direction<--1}
  if(grepl(whitePattern,position[game_pgn,square])){direction<-1}
  Col<-which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row<-as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  AttRange<-c((Col-1),(Col+1))
  AttRange<-AttRange[0<AttRange & AttRange<=8]
  AttSq<-paste(letters[AttRange],Row+direction,sep="")
  ifelse((direction==-1 & Row==7)|(direction==1 & Row==2),distance<-1:2,distance<-1)
  # if the pawn hasn't moved, it has the option of moving two spaces; otherwise, just one.
  normSq<-paste(letters[Col],Row+(distance*direction),sep="") # the pawn moves one or two spaces
  AttTest<-function(AttSquare,fromSquare=square){
    ifelse((grepl(blackPattern,fromSquare) & grepl(whitePattern,AttSquare)) |
             (grepl(whitePattern,fromSquare) & grepl(blackPattern,AttSquare)),
           T,F)
  }
  normTest<-function(normSquare,fromSquare=square){
    DF<-betweens(normSquare,fromSquare,game_pgn)
    DF<-DF[DF$squares!=fromSquare,]
    clear<-unlist(lapply(DF$occupants,is.na))
    sum(clear)==nrow(DF)
  }
  
  AttSq<-AttSq[unlist(lapply(AttSq,AttTest))]
  normSq<-normSq[unlist(lapply(normSq,normTest))]
  ifelse(0<length(c(normSq,AttSq)),
         return(c(normSq,AttSq)),
         return(NA))
}

knightPost.<-function(square,game_pgn){
  if(!grepl(knightPattern,position[game_pgn,square])){stop("this piece is not a knight")}
  Col<-which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row<-as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  NRange<-function(center,dist){ # create 1-dim function for plus & minus board distance
    dist<-unique(c(dist,dist*-1)) # distance = plus and minus displacement
    N<-expand.grid(center,dist) # combinations of center and displacements
    N<-apply(N,1,sum) # add center and displacements for 1-d location
    N[0<N & N<=8] # return only what exists within board boundaries
  }
  Path<-rbind(expand.grid(letters[NRange(Col,1)],NRange(Row,2)), # generate 1xCol and 2xRow
              expand.grid(letters[NRange(Col,2)],NRange(Row,1))) # generate 2xCol and 1xRow
  Path<-paste(Path[,1],Path[,2],sep="") # combine coordinates into squares
  Test<-function(toSquare,fromSquare=square){
    testPattern<-ifelse(grepl(blackPattern,position[game_pgn,fromSquare]),
                        whitePattern,blackPattern)
    grepl(testPattern,position[game_pgn,toSquare]) |
      is.na(position[game_pgn,toSquare])
  }
  Path[unlist(lapply(Path,Test))]
}

bishopPost.<-function(square,game_pgn){
  Col<-which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row<-as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  Lim<-min(length(Col:8),length(Row:8))
  Path<-paste(letters[Col:8][1:Lim],(Row:8)[1:Lim],sep="")
  Lim<-min(length(Col:1),length(Row:1))
  Path<-c(Path,paste(letters[Col:1][1:Lim],(Row:1)[1:Lim],sep=""))
  Lim<-min(length(Col:8),length(Row:1))
  Path<-c(Path,paste(letters[Col:8][1:Lim],(Row:1)[1:Lim],sep=""))
  Lim<-min(length(Col:1),length(Row:8))
  Path<-c(Path,paste(letters[Col:1][1:Lim],(Row:8)[1:Lim],sep=""))
  Path<-Path[Path!=square]
  Path<-sort(unique(Path))
  Test<-function(toSquare,fromSquare=square){
    fromColor<-squareColor(fromSquare,game_pgn)
    if(!is.na(fromColor)){
      if(fromColor==whitePattern){colorPattern<-blackPattern}
      if(fromColor==blackPattern){colorPattern<-whitePattern}
    }else{colorPattern<-blackPattern}
    toColor<-squareColor(toSquare,game_pgn)
    DF<-betweens(toSquare,fromSquare,game_pgn)
    T1<-(sum(is.na(DF$occupants[-c(1,nrow(DF))]))==nrow(DF)-2)
    T2a<-grepl(colorPattern,DF[DF$squares==toSquare,"occupants"])
    T2b<-is.na(DF[DF$squares==toSquare,"occupants"])
    T1 & (T2a | T2b)
  }
  Path[unlist(lapply(Path,Test))]
}

rookPost.<-function(square,game_pgn){
  Col<-which(letters==strsplit(square,"")[[1]][1]) # identify the column
  Row<-as.numeric(strsplit(square,"")[[1]][2]) # identify the row
  Path<-unique(c(paste(letters[Col],1:8,sep=""),paste(letters[1:8],Row,sep="")))
  Path<-sort(Path[Path!=square])
  Test<-function(toSquare,fromSquare=square){
    fromColor<-squareColor(fromSquare,game_pgn)
    if(!is.na(fromColor)){
      if(fromColor==whitePattern){colorPattern<-blackPattern}
      if(fromColor==blackPattern){colorPattern<-whitePattern}
    }else{colorPattern<-blackPattern}
    toColor<-squareColor(toSquare,game_pgn)
    DF<-betweens(toSquare,fromSquare,game_pgn)
    (sum(is.na(DF$occupants[-c(1,nrow(DF))]))==nrow(DF)-2) &
      (grepl(colorPattern,DF[DF$squares==toSquare,"occupants"]) |
         is.na(DF[DF$squares==toSquare,"occupants"]))
  }
  Path[unlist(lapply(Path,Test))]
}
