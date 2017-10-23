movenumber<-function(pgn){
  read.table(textConnection(pgn),sep=".")[1,1]
}