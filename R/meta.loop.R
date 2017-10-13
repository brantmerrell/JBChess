meta.loop <- function(ids,Var="Event",workdir="C:/Users/Josh/Documents"){
  if(tolower(Var)=="event"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$Event)}
  }
  if(tolower(Var)=="site"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$Site)}
  }
  if(tolower(Var)=="date"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$Date)}
  }
  if(tolower(Var)=="white"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$White)}
  }
  if(tolower(Var)=="black"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$Black)}
  }
  if(tolower(Var)=="result"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$Result)}
  }
  if(tolower(Var)=="whiteelo"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$WhiteElo)}
  }
  if(tolower(Var)=="blackelo"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$BlackElo)}
  }
  if(tolower(Var)=="timecontrol"){
    FUN <- function(id){as.vector(read.meta(id,workdir)$TimeControl)}
  }
  unlist(lapply(ids,FUN))
}
