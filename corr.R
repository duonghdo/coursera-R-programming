corr <- function(directory, threshold=0){
	files_list <- list.files(directory, full.names=TRUE)
	tmp <- lapply(files_list, read.csv)

	input <- complete(directory)
	output <- numeric()
	for (i in 1:332){
		if(input[i, 2] > threshold){
			output <- c(output, cor(tmp[[i]]$sulfate, tmp[[i]]$nitrate, use="complete.obs"))
		}
	}

	output
}