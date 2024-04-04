# AUX FUNCTIONS
######################
readWeatherFun <- function(i) { #read CSV data
  read.csv(paste(weather_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE)
}

readEnergyFun <- function(i) { #read CSV data
  read.csv(paste(data_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE, sep = ";")
}

#########################

# Prepare data path
data_path <- "data"
weather_path <- paste(data_path,"Weather", sep = "/")

# Read files
energy_data <- c(list.files(data_path, pattern = ".csv"))
weather_data <- c(list.files(weather_path, pattern = ".csv"))
weather_data <- do.call(rbind, lapply(weather_data, readWeatherFun ))
energy_data <- do.call(rbind, lapply(energy_data, readEnergyFun ))

weather_data <- as.data.frame(weather_data)
energy_data <- as.data.frame(energy_data)

# Join power consumption dataset to weather dataset
# by timestamp and where ZIP Code is 1xxx (Lisbon ZIP code)
# Discard redundant columns (ie: Timestamp on both datasets is equal)

# 1 - Convert Timestamps to same format
# Take Date and Hour features and join them in the same format as weather dataset
energy_data$Date.Time <- apply(energy_data, MARGIN = 1, function(row) paste(row["Date"], "T", row["Hour"], ":00", sep = ""))

# TODO: MERGE BOTH SHITS


