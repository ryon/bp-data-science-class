pollutantmean <- function(directory, pollutant, id = 1:332) {
  # get list of CSV files in `directory`
  data_files <- list.files(directory, "*.csv", full.names = TRUE)
  
  # init storage for our data
  pollutant_data <- numeric()
  
  # go through each specified file and add values for `pollutant`
  for (i in id) {
    data_file <- read.csv(data_files[i])
    pollutant_data <- c(pollutant_data, data_file[[pollutant]])
  }
  
  # average the values, disregarding NA
  mean(pollutant_data, na.rm = TRUE)
}