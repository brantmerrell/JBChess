mobilePlot <- function(mobilePattern="total", DF=mobility, Legend=T, legendX="topright"){
		mobileCol <- grep(mobilePattern, colnames(DF))
		plot(as.ts(DF[, mobileCol[1]]), col = "red", ylim = c(0, 
				max(c(DF[, mobileCol[1]], DF[, mobileCol[2]]))), ylab = "mobility", 
				xlab = paste(unique(gsub("white|black", "", colnames(DF)[mobileCol])), 
						collapse = ","))
		lines(as.ts(DF[, mobileCol[2]]), col = "blue")
		if (Legend) {
				legend(legendX, legend = c("white", "black"), fill = c("red", 
						"blue"))
		}
}
