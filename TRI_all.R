library(tidyverse)

# create a data frame for each year's csv file
csv_files <- list.files(pattern = "*.csv")

data_frames <- list()

for (file in csv_files){
  file_path <- file
  df_name <- sub(".csv", "", file)
  data_frames[[df_name]] <- read.csv(file_path)
}

all_TRI <- do.call(rbind, data_frames)

# Fix column names
colnames(all_TRI) <- gsub("^X\\d+\\.+", "", colnames(all_TRI))

# Reset index names
row.names(all_TRI) <- NULL

TRI_10to21 <- all_TRI %>% 
  filter(YEAR >= 2010)

# Export all_TRI as a csv
write.csv(TRI_10to21, file="TRI_10to21.csv", row.names = FALSE)

