download.pgn<-function(id,update=FALSE){
  if(nchar(id)==9){
    URL<-paste("http://www.chess.com/echess/download_pgn?id=",id,sep="")
  }
  if(nchar(id)==10){
    URL<-paste("http://www.chess.com/echess/download_pgn?lid=",id,sep="")
  }
  filepath<-paste("C:/Users/Josh/Documents/",id,".pgn",sep="")
  if(file.exists(filepath)==FALSE | update==TRUE){
    download.file(URL,filepath)
  }
}
