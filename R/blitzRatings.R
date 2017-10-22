extractBlitz<-function(Get,pattern="\\thinkboolean"){
  live_blitz<-readLines("live_blitz.txt")
  pattern<-"\\tthinkboolean"
  live_blitz<-live_blitz[grep(pattern,live_blitz)] # chr [1:1927]
  if(grepl("[Ww][Hh][Ii][Tt][Ee]",Get) & !grepl("[Rr]ating",Get)){
    Extract<-function(Line){
      strsplit(unlist(strsplit(live_blitz[Line],"\\t"))[2]," ")[[1]][1]
    }
  }else{
    if(grepl("[Ww][Hh][Ii][Tt][Ee]",Get)){
      Extract<-function(Line){
        as.integer(gsub("\\(|\\)",
                        "",
                        strsplit(unlist(strsplit(live_blitz[Line],"\\t"))[2]," ")[[1]][2]))
      }
    }
  }
  if(grepl("[Bb][Ll][Aa][Cc][Kk]",Get) & !grepl("[Rr]ating",Get)){
    Extract<-function(Line){
      strsplit(unlist(strsplit(live_blitz[Line],"\\t"))[3]," ")[[1]][1]
    }
  }else{
    if(grepl("[Bb][Ll][Aa][Cc][Kk]",Get)){
      Extract<-function(Line){
        as.integer(gsub("\\(|\\)",
                        "",
                        strsplit(unlist(strsplit(live_blitz[Line],"\\t"))[3]," ")[[1]][2]))
      }
    }
  }
  if(grepl("[Tt]hinkboolean",Get) & !grepl("[Rr]ating",Get)){
    Extract<-function(Line){
      tb<-grep("thinkboolean",strsplit(live_blitz[Line],"\\t")[[1]])
      if(tb==3){return("black")}
      if(tb==2){return("white")}
    }
  }else{
    if(grepl("[Tt]hinkboolean",Get)){
      Extract<-function(Line){
        tb<-grep("thinkboolean",strsplit(live_blitz[Line],"\\t")[[1]])
        return(as.numeric(gsub("[[:alpha:]]| |[[:punct:]]",
                               "",
                               strsplit(live_blitz[Line],"\\t")[[1]][tb])))
      }
    }
  }
  if(grepl("[Oo][Pp][Pp]",Get)){
    Extract<-function(Line){
      Line<-strsplit(live_blitz[Line],"\\t")[[1]]
      tb<-grep("thinkboolean",Line)
      ifelse(tb==2,opp<-3,opp<-2)
      return(as.numeric(strsplit(Line[opp],"\\(|\\)")[[1]][2]))
    }
  }
  if(grepl("[Rr]esult",Get)){
    Extract<-function(Line){
      strsplit(live_blitz[Line],"\\t")[[1]][5]
    }
  }
  if(grepl("[Dd]ate",Get)){
    Extract<-function(Line){
      as.character(strptime(strsplit(live_blitz[Line],"\\t")[[1]][7],"%m/%d/%y "))
    }
  }
  if(grepl("[Tt]ime",Get)){
    Extract<-function(Line){
      strsplit(live_blitz[Line],"\\t")[[1]][4]
    }
  }
  if(grepl("[Mm]oves",Get)){
    Extract<-function(Line){
      as.numeric(strsplit(live_blitz[Line],"\\t")[[1]][6])
    }
  }
  unlist(lapply(1:length(live_blitz),Extract))
}
liveBlitz<-data.frame(White=extractBlitz("white"), WhiteRating=extractBlitz("white rating"),
                      Black=extractBlitz("black"), BlackRating=extractBlitz("black rating"),
                      Me=extractBlitz("Thinkboolean"),
                      MyRating=extractBlitz("thinkbooleanRating"), OppRating=extractBlitz("Opp"),
                      Result=extractBlitz("result"), Time=extractBlitz("Time"),
                      Moves=extractBlitz("Moves"), Date=extractBlitz("Date"))
average<-function(DF=liveBlitz,Var="BlackRating",aveType="daily"){
  if(grepl("[Dd]aily",aveType)){
    L<-levels(DF$Date)
    D<-data.frame(Date=seq.Date(as.Date(L[1]),as.Date(L[length(L)]),by=1),
                  startRating=NA,medianRating=NA,meanRating=NA,endRating=NA,
                  row.names="Date")
    for(Date  in L){
      DF2<-DF[DF$Date==Date,]
      D[Date,]<-c(startRating=DF2[1,Var],
                  medianRating=median(DF2[,Var]),
                  meanRating=mean(DF2[,Var]),
                  endRating=DF2[nrow(DF2),Var])
    }
    for(Date in row.names(D)){
      if(sum(is.na(D[Date,]))==4){
        D[Date,]<-D[as.character(as.Date(Date)-1),"endRating"]
      }
    }
    return(D)
  }
}
D<-average()
D<-data.frame(Date=row.names(D),D,row.names=NULL)
D<-D["2015-05"<as.vector(D$Date),]
D$Date<-factor(D$Date)


jpeg("temp.jpeg")
plot(D$Date,D$endRating)
dev.off()

