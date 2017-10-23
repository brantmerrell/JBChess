# create a board:
position <- setup()[1,]

# look at the board:
board()

# pick a first square:
firstSquare<-colnames(position)[sample(1:64,1)]

# start tally
tally <- 1

# place knight with tally:
position[,firstSquare]<-paste("white knight",tally)

# look at the board:
board()

# list where the knight can move:
options <- knightPost.(firstSquare,nrow(position))

# look at the list:
options

# pick a move:
move <- sample(options,1)

# look at the move:
move

# create next position (current position then add piece)
nextPosition <- position[nrow(position),] # current position
tally <- tally + 1
nextPosition[,move] <- paste("white knight",tally) # add piece

# add next position to position data frame:
position <- rbind(position,nextPosition)

# look at the board:
board()

# repeat
options <- knightPost.(move,nrow(position))
options
move <- sample(options,1)
move
nextPosition <- position[nrow(position),]
tally <- tally + 1
nextPosition[,move] <- paste("white knight",tally)
position <- rbind(position,nextPosition)
board()

Repeat<-function(){
  options <- knightPost.(move,nrow(position))
  options
  move <- sample(options,1)
  move
  nextPosition <- position[nrow(position),]
  tally <- tally + 1
  nextPosition[,move] <- paste("white knight",tally)
  position <- rbind(position,nextPosition)
  board()
  
}

Repeat()
