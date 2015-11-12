complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	
	result_data <- data.frame()

	for (i in 1:length(id))
	{
		filenames <- paste(directory, "/", formatC(id[i], width=3, flag="0"), ".csv", sep = "")
		v_data <- read.csv(filenames)
		result_data <- rbind(result_data, c(id[i], sum(complete.cases(v_data))))
	}	
	
	names(result_data) <- c("id", "nobs")
	return(result_data)	
}
