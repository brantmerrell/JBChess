pgn.xy<-function(pgn){
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
  x<-as.character(subset(df$x,df$assess==TRUE))
  y<-as.character(subset(df$y,df$assess==TRUE))
  sq<-as.character(subset(df$sq,df$assess==TRUE))
  return(list(x=x,y=y,sq=sq))
}