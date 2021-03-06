best <- function(state, outcome){
	data <- read.csv("outcome-of-care-measures.csv")

	if(!state %in% data[,"State"]) {
		stop("invalid state")
	}
	valid_out <- data.frame(out = c("heart attack", "heart failure", "pneumonia"),
							index = c(11, 17, 23))
	if(!outcome %in% valid_out$out){
		stop("invalid outcome")
	}
	valid_out <- subset(valid_out, valid_out$out == outcome)

	result <- filter(data, State == state)
	result <- select(result, Hospital.Name, valid_out$index)
	names(result) <- c("Hospital", "Rate")
	result <- filter(result, Rate != "Not Available")
	result$Rate <- as.numeric(as.character(result$Rate))
	result <- arrange(result, Rate)
	#result
	as.character(result[1,1])
}