corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
	library(dplyr)	
	
	source("complete.R")
	c <- complete(directory)
	id <- c[c$nobs > threshold, "id"]
	
	if (length(id) > 0)
	{
		filenames <- paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep = "")
		v_data <- do.call(rbind, lapply(filenames, read.csv))
		v_data <- v_data[complete.cases(v_data), ]

		id.group <- group_by(v_data, ID)
		id.cor <- summarize(id.group, cor = cor(sulfate, nitrate))

		id.cor <- data.frame(id.cor)

		return(id.cor[, "cor"])
	}
	else return(NULL)
}
