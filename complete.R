complete <- function(directory, id = 1:332){
	files_list <- list.files(directory, full.names=TRUE)
	#tmp <- vector(mode = "list", length = length(files_list))
	#for (i in seq_along(files_list)){
	#	tmp[[i]] <- read.csv(files_list[[i]])
	#}
	tmp <- lapply(files_list, read.csv)
	output <- data.frame(integer(), integer())
	for (i in id){
		good <- complete.cases(tmp[[i]])
		#output[i, 1] = i
		#output[i, 2] = nrow(tmp[[i]][good, ])
		output <- rbind(output, c(i, nrow(tmp[[i]][good, ])))
	}
	names(output) <- c("id", "nobs")
	output
}