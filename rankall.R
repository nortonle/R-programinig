rankall <- function(outcome, num = "best") 
{
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	library(dplyr)	
	library(gtools)

	v_state <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL',	'GA',	'GU',	'HI',	'IA',	'ID',	'IL',	'IN',	'KS',	'KY',	'LA',	'MA',	'MD',	'ME',	'MI',	'MN',	'MO',	'MS',	'MT',	'NC',	'ND',	'NE',	'NJ',	'NM',	'NV',	'NY',	'NH',	'OH',	'OK',	'OR',	'PA',	'PR',	'RI',	'SC',	'SD',	'TN',	'TX',	'UT',	'VA',	'VI',	'VT',	'WA',	'WI',	'WV',	'WY')
	v_outcome <- c('heart attack', 'heart failure', 'pneumonia')	
	v_result <- data.frame()

	if (!(outcome %in% v_outcome)) stop("Invalid outcome")
	
	if (outcome == "heart attack")
	{
		v_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
				filter(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available') %>%
				select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		v_data[, 3] <- as.numeric(v_data[, 3])
		arrange_data <- arrange(v_data, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
	}
	if (outcome == "heart failure")
	{
		v_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
				filter(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available') %>%
				select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		v_data[, 3] <- as.numeric(v_data[, 3])
		arrange_data <- arrange(v_data, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
	}
	if (outcome == "pneumonia")
	{
		v_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
				filter(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available') %>%
				select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		v_data[, 3] <- as.numeric(v_data[, 3])
		arrange_data <- arrange(v_data, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)
	}
	
	split_data <- split(arrange_data, arrange_data$State)
	for (i in 1:length(v_state))
	{
		if (num == "best")
		{
			v_result <- smartbind(v_result, data.frame(split_data[[i]][1, 2], v_state[i]))
		}
		else if (num == "worst")
		{
			v_result <- smartbind(v_result, data.frame(split_data[[i]][nrow(split_data[[i]]), 2], v_state[i]))
		}
		else
		{
			v_result <- smartbind(v_result, data.frame(split_data[[i]][as.numeric(num), 2], v_state[i]))
		}
	}
	
	names(v_result) <- c("hospital", "state")
	return(v_result)
}
