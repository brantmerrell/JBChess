weave.rbind <- function(DF1, DF2){
		for(n in 1:ncol(DF1)){
		if(!colnames(DF1)[n] %in% colnames(DF2)){
			DF2[,colnames(DF1)[n]] <- ""
			}
		}
		for(n in 1:ncol(DF2)){
		if(!colnames(DF2)[n] %in% colnames(DF1)){
			DF1[,colnames(DF2)[n]] <- ""
			}
		}
		return(rbind(DF1,DF2, stringsAsFactors=F))
	}
