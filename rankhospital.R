rankhospital <- function(state, outcome, num = "best") 
{
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with the given rank
	## 30-day death rate

	library(dplyr)	

	v_state <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL',	'GA',	'GU',	'HI',	'IA',	'ID',	'IL',	'IN',	'KS',	'KY',	'LA',	'MA',	'MD',	'ME',	'MI',	'MN',	'MO',	'MS',	'MT',	'NC',	'ND',	'NE',	'NJ',	'NM',	'NV',	'NY',	'NH',	'OH',	'OK',	'OR',	'PA',	'PR',	'RI',	'SC',	'SD',	'TN',	'TX',	'UT',	'VA',	'VI',	'VT',	'WA',	'WI',	'WV',	'WY')
	v_outcome <- c('heart attack', 'heart failure', 'pneumonia')	

	if (!(state %in% v_state)) stop("Invalid state")
	if (!(outcome %in% v_outcome)) stop("Invalid outcome")

	if (outcome == "heart attack")
	{
		v_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
				filter(State == state 
					& Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available') %>%
				select(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		v_data[, 2] <- as.numeric(v_data[, 2])
		v_data <- arrange(v_data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
	}
	if (outcome == "heart failure")
	{
		v_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
				filter(State == state 
					& Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available') %>%
				select(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		v_data[, 2] <- as.numeric(v_data[, 2])
		v_data <- arrange(v_data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
	}
	if (outcome == "pneumonia")
	{
		v_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
				filter(State == state 
					& Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available') %>%
				select(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		v_data[, 2] <- as.numeric(v_data[, 2])
		v_data <- arrange(v_data, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)
	}

	if (num == "best") return(v_data[1,1])
	if (num == "worst") return(v_data[nrow(v_data), 1])
	if (as.numeric(num) > nrow(v_data)) return(NA)
	return(v_data[as.numeric(num), 1])
}
