testId <- function(id=1000,type="correspondence"){
  if(type=="correspondence"){
    Url <- "http://www.chess.com/echess/game?id="
    Url <- paste(Url,id,sep="")
    L <- length(readLines(Url))
    if(L==1059){return(T)}
    if(L==798){return(F)}
    if(!(L %in% c(798,1059))){return(NA)}
  }
  if(type=="live"){
    Url <- "http://www.chess.com/livechess/game?id="
    Url <- paste(Url,id,sep="")
    L <- length(readLines(Url))
    if(L==1008){return(T)}
    if(L==798){return(F)}
    if(!(L %in% c(798,1008))){return(NA)}
  }
}
