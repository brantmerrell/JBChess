# A function (in testing) for scraping chess.com information which requires a login. 

# It is unclear whether 'readLines(link)' yields more information as a result of obtaining a username/password response. 

library(httr)
library(XML)

login <- function(link, 
			path="login"){
	user=readline("username:")
	pass=readline("password:")
	hnd <- paste(unlist(strsplit(link,"/")[[1]][1:3]), collapse="/")
		# it is unclear whether this will collapse into the correct number of forward slashes
	print(hnd)
#	print(hnd)
	response <- POST(handle=hnd, path=path, 
			body=list(login=user,pass=pass,redirect_url=link))
	response
}

