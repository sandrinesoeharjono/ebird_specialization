### Calculate temporal changes in beta diversity across the CMM ###

# load packages and files into environment
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
fullDataset <- read.csv("final_shortened_fullDatasets.csv")

# create empty array richness of length 981 (to calculate richness value per pair of windows)
richness <- data.frame(matrix("", nrow = 981, ncol = 2), stringsAsFactors = FALSE)
colnames(richness) = c("Richness", "Avg_Date")

# look at fullDataset but transformed into character class & Date
fullDataset_char <- fullDataset %>%
  mutate_all(.funs = funs(as.character)) %>% 
  glimpse

fullDataset_date <- fullDataset_char %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  glimpse

one_day <- interval(start = "1990-01-01", end = "1990-01-02")

####### create a list of intervals (not the same size, between each 1st of the month) ######
# starts of each year
obs_years <- fullDataset_date$eventDate %>% 
  unique %>% 
  lubridate::year() %>% 
  unique

# starts of each month
first_days <- obs_years %>%
  purrr::map(~ paste0(., "-", 1:12)) %>% 
  paste0("-01")

first_day = as.numeric(as.Date(min(fullDataset_date$eventDate)))
last_day = as.numeric(as.Date(max(fullDataset_date$eventDate)))
window_size = 60
window_shift = 10
start = seq(first_day, last_day-window_size-1, window_shift)
end = seq(first_day+window_size, last_day, window_shift)

# create window_table_2m showing intervals of 2 months
window_table_2m <- data_frame(
  start = as.Date(start, origin = "1970-01-01"),
  end = as.Date(end, origin = "1970-01-01")
)

# mutate fullDatasets_2m to separate into the intervals above and put into filtered_datasets column
fullDatasets_2m <- window_table_2m %>% 
  mutate(time_interval = interval(start, end),
         # create a list-column of all the subsets of the data that fall in the interval
         filtered_datasets = time_interval %>% 
           purrr::map(~ filter(fullDataset_date, eventDate %within% .)))
rm(fullDataset_char, fullDataset_date) # remove from environment

##### calculating # unique species (richness) in each individual window
for(i in 1:nrow(fullDatasets_2m)){      # length of 981
  currentwindow <- fullDatasets_2m$filtered_datasets[[i]]
  
  # array of unique species in dataset (length = # beta)
  richness[i,1] <- length(unique(currentwindow$species))    # place in array
  
  # calculate average date for each window
  richness[i,2] <- toString(mean(as.Date(currentwindow$eventDate)))
}

##### calculating # unique species (richness) in each PAIR of windows
# creating empty array "species_paired_windows" 
species_paired_windows <- data.frame(matrix("", nrow = 980, ncol = 1), stringsAsFactors = FALSE)

for(i in 1:(nrow(fullDatasets_2m)-1)){      # length of 980
  currentwindow <- fullDatasets_2m$filtered_datasets[[i]]$species
  nextwindow <- fullDatasets_2m$filtered_datasets[[i+1]]$species
  
  # species in both windows
  both <- append(currentwindow, nextwindow)
  
  # array of unique species in dataset (length = # beta)
  species_paired_windows[i,1] <- length(unique(both))    # place in array
}

##### calculating beta-diversity
# create empty array beta of length 980 (to calculate 1 beta-diversity value per pair of windows)
beta <- data.frame(matrix("", nrow = 980, ncol = 2), stringsAsFactors = FALSE)
colnames(beta) = c("Beta_values", "Avg_Date")

# calculate beta-diversity index for each pair of windows (980 times)
for(j in 1:(nrow(richness)-1)){
  beta[j,1] <- (as.numeric(richness[j,1]) + as.numeric(richness[j+1,1])) / (2 * as.numeric(species_paired_windows[j,1])) # beta value
  beta[j,2] <- toString(as.Date(richness[j,2]) + 30)  # average date between both frames (+30 days exactly)
}

# save CSV file "beta.csv" (# unique species in each window w/ the average date)
write.csv(beta, file = "beta.csv", row.names = FALSE)
