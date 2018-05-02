
source(list.files(pattern="scrape\\.live\\.R$",full.names=T, recursive=T))
source(list.files(pattern="regex\\.scrape\\.R$",full.names=T, recursive=T))

scrape.chess.com <- function(id,type = c("live","correspondence","open")){
	type <- paste(type,collapse = "")
	
	if(grepl("open",type,ignore.case = T)){
		Link <- file.path("https://www.chess.com/games/view",id)
		DF1 <- regex.scrape(Link,"open")
		DF1 <- cbind(DF1, ID=paste("o",id,sep = "_"),stringsAsFactors=F)
	}else{
		DF1 <- data.frame(Link="NA", stringsAsFactors = F)
	}
	
	if(grepl("corres|turn|daily",type,ignore.case = T)){
		Link <- file.path("https://www.chess.com/daily/game",id)
		DF2 <- regex.scrape(Link, "correspondence")
		DF2 <- cbind(DF2, ID=paste("c",id,sep = "_"), stringsAsFactors=F)
	}else{
		DF2 <- data.frame(Link="NA", stringsAsFactors = F)
	}
	
	# if(!exists("DF2")){DF2 <- data.frame(Link, stringsAsFactors = F)}
	
	if(grepl("live",type,ignore.case = T)){
		Link <- file.path("https://www.chess.com/live/game",id)
		DF3 <- scrape.live(live.link=Link)
		DF3 <- cbind(DF3, ID=paste("l",id,sep="_"),stringsAsFactors=F)
	}else{
		DF3 <- data.frame(Link="NA", stringsAsFactors = F)
	}
	# if(!exists("DF3")){DF3 <- data.frame(Link, stringsAsFactors = F)}

	DF <- weave.rbind(DF1,DF2)
	DF <- weave.rbind(DF, DF3)
	DF <- DF[DF$White!="",]
	if(0<nrow(DF)){
		return(DF)
	}else{
		return(data.frame(ID=paste("NA",id,sep="_"),stringsAsFactors=F))
	}
}
