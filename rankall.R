rankall <- function(outcome, num = "best"){
	library(dplyr)
	data <- read.csv("outcome-of-care-measures.csv", 
					na.strings = c("Not Available", "NA"))
	if(num == "best") num <- 1L

	valid_out <- data.frame(out = c("heart attack", "heart failure", "pneumonia"),
							index = c(11, 17, 23))
	if(!outcome %in% valid_out$out){
		stop("invalid outcome")
	}
	valid_out <- subset(valid_out, valid_out$out == outcome)

	dat_fil <- subset(data, select = c(Hospital.Name, State, valid_out$index))
	names(dat_fil) <- c("hospital", "state", "rate")
	good <- complete.cases(dat_fil)
	dat_fil <- dat_fil[good, ]
	#dat_fil
	state_split <- split(dat_fil, dat_fil$state)
	#state_split
	
	state_split <- lapply(state_split, arrange, rate, hospital)
	if(num != "worst") state_res <- lapply(state_split, function(s) s[num, 1:2])
	else state_res <- lapply(state_split, function(s) s[nrow(s), 1:2])
	#state_res
	#View(state_res)
	result <- do.call("rbind", state_res)
	result$state <- row.names(result)
	result
}