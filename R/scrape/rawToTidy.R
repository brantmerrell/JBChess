rawToTidy <- function (pgnFile) {
	if (grepl("chess.com", pgnFile)) {
		if (!grepl("@", pgnFile)) {
			pgnFile <- paste("https://thinkboolean:witstorm666", 
				gsub("https://", "", pgnFile), sep = "@")
		}
		if (!grepl("livechess", pgnFile)) {
			con = url(pgnFile)
			htmlCode = readLines(con)
			close(con)
			pgn <- unlist(strsplit(htmlCode[grep("\\d\\.[[:lower:]]\\d", 
				htmlCode)], "Board\\(\\'|\\'\\)"))
			pgn <- pgn[grepl("\\d\\.[[:lower:]]\\d", pgn)]
			pgn <- strsplit(pgn, "\\+(?!\\+)", perl = T)[[1]]
			Title <- htmlCode[grep("<title>", htmlCode, ignore.case = T)]
			Title <- gsub("</?title>| {2}", "", Title)
			metaChess <- data.frame(Title, pgn = paste(pgn, collapse = " "))
		}
		else {
			message("cannot currently extract live games")
		}
	}
	else {
		pgnLines <- readLines(pgnFile)
		notationLines <- (grep("\\d\\.[KQRNB]?[abcdefgh12345678]?x?[abcdefgh][1234567]", 
			pgnLines))
		range <- data.frame(start = grep("\\[Event", pgnLines))
		range <- cbind(range, stop = c(range[-1, "start"] - 1, 
			length(pgnLines)))
		for (n in 1:nrow(range)) {
			m <- seq(range[n, "start"], range[n, "stop"])
			meta <- pgnLines[m][grep("\\[", pgnLines[m])]
			pgn <- paste(pgnLines[m][!pgnLines[m] %in% meta], 
				collapse = " ")
			pgn <- gsub(" {2,}", " ", pgn)
			meta <- unlist(strsplit(meta, "\""))
			gsub("\\[| $", "", meta[grep("^\\[", meta)])
			DF <- data.frame(matrix(meta[-grep("\\[|\\]", meta)], 
				nrow = 1))
			colnames(DF) <- gsub("\\[| $", "", meta[grep("^\\[", 
				meta)])
			DF <- cbind(DF, pgn = pgn)
			if (n == 1) {
				metaChess <- DF
			}
			else {
				DF <- DF[, colnames(DF) %in% colnames(metaChess)]
				metaChess <- rbind(metaChess, DF)
			}
		}
	}
	metaChess
}
