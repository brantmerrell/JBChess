logchess <- function(command="write"){
	linkpath <- "C:/Users/Josh/Documents/Chess/chesslinks.csv"
	logpath <- "C:/Users/Josh/Documents/chesslogs.csv"
	chesslinks <- unique(read.csv(linkpath,colClasses="character"))
	chesslogs <- data.frame(link=as.vector(chesslinks$link),
												type=as.vector(read.table(textConnection(as.vector(chesslinks$link)),sep="/")$V4),
												ID=as.vector(read.table(textConnection(as.vector(chesslinks$link)),sep="=")$V2))
	FUN <- function(id){
		read.meta(id)$Event
	}
	chesslogs <- cbind(chesslogs,Event=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		paste(strptime(read.meta(id)$Date,format="%Y.%m.%d"))
	}
	chesslogs <- cbind(chesslogs,Date=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		read.meta(id)$White
	}
	chesslogs <- cbind(chesslogs,White=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		read.meta(id)$Black
	}
	chesslogs <- cbind(chesslogs,Black=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		read.meta(id)$Result
	}
	chesslogs <- cbind(chesslogs,Result=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		read.meta(id)$WhiteElo
	}
	chesslogs <- cbind(chesslogs,WhiteElo=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		read.meta(id)$BlackElo
	}
	chesslogs <- cbind(chesslogs,BlackElo=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	FUN <- function(id){
		read.meta(id)$TimeControl
	}
	chesslogs <- cbind(chesslogs,TimeControl=as.matrix(unlist(lapply(chesslogs$ID,FUN))))
	if(command=="write"){
		write.csv(chesslogs,logpath,row.names=FALSE)
		print(tail(read.csv(logpath),3))
	}
	if(command=="return"){
		return(chesslogs)
		print(list(varnames=colnames(chesslogs),
							 rows=nrow(chesslogs),
							 columns=ncol(chesslogs)))
	}
}
