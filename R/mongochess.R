mongochess <- function(row,df=echess,dbN="local",h="localhost"){
	library(RMongo)
	doc <- paste("{",
						 "'_id':'",						 
						 paste(df[row,1],df[row,2],df[row,3],sep="_"),"','",
						 colnames(df)[1],"':'",df[row,1],"','",
						 colnames(df)[2],"':'",df[row,2],"','",
						 colnames(df)[3],"':'",df[row,3],"','",
						 colnames(df)[4],"':'",df[row,4],"','",
						 colnames(df)[5],"':'",df[row,5],"','",
						 colnames(df)[6],"':'",df[row,6],"','",
						 colnames(df)[7],"':'",df[row,7],"','",
						 colnames(df)[8],"':'",df[row,8],"','",
						 colnames(df)[9],"':'",df[row,9],						 
						 "'}",sep="")
	mongo <- mongoDbConnect(dbName=dbN, host=h)
	output <- dbInsertDocument(rmongo.object=mongo, 
														 collection="josh_chess", 
														 doc=doc)
	dbDisconnect(mongo)
}
