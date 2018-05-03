TEST <- grepl("thinkboolean", DF$White)

TEST <- TEST | grepl("thinkboolean", DF$Black)

if(0<sum(TEST)){
	DF1 <- DF[TEST,]
	print(tail(DF1))
	if(file.exists("../thinkboolean.csv")){
		DF1 <- weave.rbind(DF1, read.csv("../thinkboolean.csv"))
	}
	write.csv(DF1, "../thinkboolean.csv", row.names=F)
}

filename <- 

write.csv(DF, filename, row.names=F)

print(filename)


