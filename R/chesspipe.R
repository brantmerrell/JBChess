# make sure directory is "chessdoodles"
grepl("[Cc]hessdoodles$",getwd()) # setwd("Josh/chessdoodles")

# clear workspace:
rm(list=ls())

# load chess functions and patterns:
load(".RData")

# load random pgn files:
set.seed(123)
Files <- sample(list.files("PGN", pattern = "\\.pgn$", full.names = T), 5)

readLines(Files[1])

# load files into gameLog:
# print game summaries:
for(File in Files){
  # load the file into 'meta' data frame form,
  DF <- rawToTidy(File)
  
  # if it's the first file, store it as 'gameLog'
  if(File==Files[1]){
    gameLog <- DF
    
  # for additional files, add to existing 'gameLog' object
  }else{
    
    # remove any extra columns added (blitz games have additional categories)
    gameLog <- DF[,colnames(DF) %in% colnames(gameLog)]
    
    # add to existing object:
    gameLog <- rbind(gameLog,DF)  
  }
  
  # print a few associated variables to console:
  print(summary(DF[,c("Date","Black","BlackElo","Termination")]))
}

# remove any duplicate rows of gameLog:
gameLog<-(unique(gameLog))

# arrange gameLog according to date:
gameLog <- gameLog[order(as.vector(gameLog[,"Date"]),decreasing = F),]

# explore gameLog:
dim(gameLog)
View(gameLog)

# extract a moveLog from gameLog:
 # (print the last move of each game)
for(n in 1:nrow(gameLog)){
  
  # convert a row to a 2-column data frame of gameID and pgn:
  tempFrame<-tidyToPgn(gameLog,n)
  
  # define the first converted row as a new object:
  if(n==1){
    moveLog<-tempFrame
    
  # add additional rows to moveLog:
  }else{
    moveLog<-rbind(moveLog,tempFrame)
  }
  
  # print the last row of each game to the console:
  print(cbind(tail(tempFrame,1),n))
}

# convert moveLog from matrix to data frame:
moveLog <- as.data.frame(moveLog)

# explore moveLog:
head(moveLog) # top
tail(moveLog) # bottom
summary(moveLog)
moveLog[sample(1:nrow(moveLog),6),] # random

# assign unique gameIDs from the moveLog to the gameLog:
row.names(gameLog)<-unique(moveLog$gameID)

# inspect the rownames of gameLog:
head(gameLog[,-c(1,2,11)]) # top 
tail(gameLog[,-c(1,2,11)]) # bottom
gameLog[sample(1:nrow(gameLog),6),-c(1,2,11)] # random

# remove games that have no moves from the moveLog:
moveLog <- moveLog[!is.na(moveLog$pgn),]

# write.csv(gameLog,"metadata and pgn.csv")
# write.csv(moveLog,"gameID and moves.csv")

# build a "position" data frame with a row for each move:
# (print information for each game after it's completed)
for(ID in unique(as.vector(moveLog$gameID))){
  game <- moveLog[moveLog[,"gameID"]==ID,] 
  pgn <- as.vector(game[,"pgn"])
  if(ID == unique(as.vector(moveLog$gameID))[1]){
    position<-setup(gameName=ID, includeEmpty = F)
  }else{
    position <- rbind(position, setup(gameName=ID,includeEmpty = T))
  }
  # notate<-2
  for(notate in 1:length(pgn)){
    position<-
      rbind(position, newPosition(new_pgn = paste(ID,
                                                  as.vector(game[notate,"pgn"]),
                                                  sep="_")))
  }
  print(cbind(gameLog[ID,-c(1,2,11)],Last.Move=pgn[length(pgn)],positionRows=nrow(position)))
}

# remove any 'empty' positions:
position <- position[!grepl("empty$",row.names(position)),]

# select ten random snapshots of the chessboard, 
set.seed(123)
snapshots <- sample(row.names(position),10)

# build corresponding mobility scores:
for(n in snapshots){
  piecePatterns<-c(pawns=pawnPattern,knights=knightPattern,bishops=bishopPattern,
                   rooks=rookPattern,queens=queenPattern,kings=kingPattern)
  DF <- mobility2(n)
  colnames(DF)<-paste(colnames(DF),"total")
  for(m in 1:length(piecePatterns)){
    newDF<-mobility2(n,piecePatterns[m])
    colnames(newDF)<-paste(colnames(newDF),names(piecePatterns)[m])
    DF <- cbind(DF, newDF)
  }
  if(n==snapshots[1]){mobility <- DF}else{
    mobility <- rbind(mobility,DF)
  }
  print(DF)
}

View(mobility)
View(board(row.names(mobility)[3]))

# add "mean mobility" for each game
