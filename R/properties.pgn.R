properties.pgn<-function(pgn){
  chesscolor<-function(pgn){
    if(ncol(read.table(textConnection(pgn),sep="."))==4){
      return("black")
    }
    if(ncol(read.table(textConnection(pgn),sep="."))==2){
      return("white")
    }
    if(!(ncol(read.table(textConnection(pgn),sep=".")) %in% c(2,4))){
      stop("invalid number of dots")
    }
  }
  movenumber<-function(pgn){
    read.table(textConnection(pgn),sep=".")[1,1]
  }
  piece<-function(pgn){
    test<-function(L){grepl(L,pgn)}
    P<-c("K","Q","R","N","B","O")
    Piece=c("King","Queen","Rook","Knight","Bishop","King & Rook")
    df<-data.frame(P=P, Piece=Piece,assess=unlist(lapply(P,test)))
    if(sum(df$assess)==0){
      return("pawn")
    }
    if(sum(df$assess)==1){
      return(as.character(subset(df$Piece,df$assess==TRUE)))
    }
    if(1<sum(df$assess)){
      stop("Input contains extra piece")
    }
  }
  upboard<-function(x){
    if(!(tolower(x) %in% c(1:8,letters[1:8]))){
      stop("invalid X coordinate")
    }
    if(tolower(x) %in% letters[1:8]){
      return(paste(tolower(x),1:8,sep=""))
    }
    if(x %in% 1:8){
      return(paste(letters[x],1:8,sep=""))
    }
  }
  test<-function(sq){grepl(sq,pgn)}
  sq<-unlist(lapply(1:8,upboard))
  replic8<-function(x){
    replicate(8,x)
  }
  x<-unlist(lapply(letters[1:8],replic8))
  y<-rep(1:8,times=8)
  df<-data.frame(sq=sq, x=x,y=y,assess=unlist(lapply(sq,test)))
  if(sum(df$assess)!=1){
    stop("unrecognized coordinates")
  }
  xcoor<-function(pgn){
    return(as.character(subset(df$x,df$assess==TRUE)))
  }
  ycoor<-function(pgn){
    return(as.character(subset(df$y,df$assess==TRUE)))
  }
  sqcoor<-function(pgn){
    return(as.character(subset(df$sq,df$assess==TRUE)))
  }
  return(list(pgn=pgn,
              color=chesscolor(pgn),
              move=movenumber(pgn),
              piece=piece(pgn),
              x=xcoor(pgn),
              y=ycoor(pgn),
              sq=sqcoor(pgn)))
}