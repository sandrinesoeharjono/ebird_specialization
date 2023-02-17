# Set up environment
library(plyr)
library(dplyr)
library(lubridate)
library(purrr)

# Reading the observation datasets from eBird csv files
Data1 <- read.csv(file="1990-1995.csv", header=TRUE, sep="\t")
Data2 <- read.csv(file="1996-2000.csv", header=TRUE, sep="\t")
Data3 <- read.csv(file="2001-2004.csv", header=TRUE, sep="\t")
Data4 <- read.csv(file="2005-2008.csv", header=TRUE, sep="\t")
Data5 <- read.csv(file="2009-2012.csv", header=TRUE, sep="\t")
Data6 <- read.csv(file="2013.csv", header=TRUE, sep="\t")
Data7 <- read.csv(file="2014.csv", header=TRUE, sep="\t")
Data8 <- read.csv(file="2015.csv", header=TRUE, sep="\t")
Data9 <- read.csv(file="2016.csv", header=TRUE, sep="\t")

# Read CSV "municipalites"
muni <- read.csv(file="municipalites.csv", header=TRUE, sep=",")

# join all 9 Datasets into one FullDatasets
fullDatasets <- rbind(Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9)
rm(Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9)

# Add "municipalite" column to obtain the CMM municipality of each observation from shapefile polygons
source("Read_Municipalites.R")
fullDatasets$municipalite = getMunicipalite(fullDatasets)

# Save full file
write.csv(fullDatasets, file="fullDatasets.csv",row.names=FALSE)

# select only columns of interest & save shortened file
fullDatasets <- fullDatasets %>% 
  select(species, eventDate, recordedBy, municipalite)
write.csv(fullDatasets, file="shortened_fullDatasets.csv", row.names=FALSE)

# count & remove entries with NA for municipalite
plyr::count(fullDatasets$municipalite == "NA")
fullDatasets <- fullDatasets[!is.na(fullDatasets$municipalite), ] 
write.csv(fullDatasets, file="fullDatasets_omit.csv",row.names=FALSE)    # save shortened NA-omitted file

# organize in order of date (eventDate variable)
fullDatasets <- fullDatasets[order(fullDatasets$eventDate),] 

################## calculating biodiversity indices through windows of 2 months ######################
# separate fullDatasets into 981 sets (sliding windows of 30 days, shifts of 10 days between beginnings)
# 1st column = CSI, 2nd column = average Date for each window
CSI_changes_2m <- data.frame(matrix("", nrow = 981, ncol = 2), stringsAsFactors = FALSE)

# transform columns of fullDatasets into character & date classes
fullDatasets <- fullDatasets %>% 
  mutate_all(.funs = funs(as.character)) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  glimpse

one_day <- interval(start = "1990-01-01", end = "1990-01-02")

####### create a list of intervals (not the same size, between each 1st of the month) ######
# starts of each year
obs_years <- fullDatasets$eventDate %>% 
  unique %>% 
  lubridate::year() %>% 
  unique

# starts of each month
first_days <- obs_years %>%
  map(~ paste0(., "-", 1:12)) %>% 
  flatten_chr %>% 
  paste0("-01")
rm(obs_years)

first_day = as.numeric(as.Date(min(fullDatasets$eventDate)))
last_day = as.numeric(as.Date(max(fullDatasets$eventDate)))
window_size = 60
window_shift = 10
start = seq(first_day, last_day-window_size-1, window_shift)
end = seq(first_day+window_size, last_day, window_shift)

# create window_table_2m showing intervals of 2 months
window_table_2m <- data.frame(
  start = as.Date(start, origin="1970-01-01"),
  end = as.Date(end, origin="1970-01-01")
)

# mutate fullDatasets_2m to separate into the intervals above and put into filtered_datasets column
fullDatasets_2m <- window_table_2m %>% 
  mutate(time_interval = interval(start, end),
         # create a list-column of all the subsets of the data that fall in the interval
         filtered_datasets = time_interval %>% 
           map(~ filter(fullDatasets, eventDate %within% .)))

# calculate SSI and CSI for each window
for(i in 1:nrow(fullDatasets_2m)){      # length of 981
  currentwindow <- fullDatasets_2m$filtered_datasets[[i]]
  
  # array of unique species in dataset (length = # unique species)
  unique_species <- unique(currentwindow$species)
  
  # count the number of distinct municipalities for each species --> new temporary object spe_mun
  spe_mun <- ddply(currentwindow, ~species, summarise, distinct_municipalities=length(unique(municipalite)))
  spe_mun$SSI <- NA      # add empty new column to spe_mun
  
  # CALCULATING SSI (going through entire set to calculate it for each unique species)
  # spe_mun$SSI <- sqrt(82/spe_mun$distinct_municipalities - 1)
  unique_m = length(unique(unlist(currentwindow$municipalite)))
  spe_mun$SSI <- sqrt(unique_m/spe_mun$distinct_municipalities - 1)
  
  # save CSV file -- make interval a character and add to filename
  write.csv(spe_mun, file = paste('spe_mun_season', i, 'csv', sep = '.'), row.names=FALSE)
  
  # CALCULATING CSI
  sum_SSI <- sum(spe_mun$SSI)
  num_species <- length(unique_species)
  CSI_changes_2m[i,1] <- sum_SSI/num_species
  
  # calculate average date for each window
  CSI_changes_2m[i,2] <- toString(mean(as.Date(currentwindow$eventDate)))
  
  # saving 
  nam <- paste("spe_mun", i, sep = "")    # create new variable "spe_mun + i"
  assign(nam,spe_mun)                     # assign spe_mun value to new variable
  spe_mun <- NULL                         # delete spe_mun
}

# save CSI for each window in csv file
colnames(CSI_changes_2m) <- c("CSI", "Average_Date")
write.csv(CSI_changes_2m, file = "CSI_changes_2m.csv")

# dataframe comparing 3 variables (num_species, num_municipalities, num_observations) per average window date
changes_w <- data.frame(integer(), integer(), integer(), integer(), stringsAsFactors=FALSE)
colnames(changes_w) <- c("Number_species", "Number_municipalities", "Number_observations", "Avg_Date")
for (i in c(1:981)){
  current_window <- fullDatasets_2m$filtered_datasets[[i]]
  num_spe <-length(unique(current_window$species))
  num_mun <- length(unique(current_window$municipalite))
  num_obs <- nrow(current_window)
  date <- toString(mean(as.Date(current_window$eventDate)))
  changes_w[i,] <- c(num_spe, num_mun, num_obs, date)
}
write.csv(changes_w, file = "changes_w.csv")

###################################### basic plotting of results #####################################

# 1) plot CSI by average window date
plot(CSI_changes_2m$CSI ~ as.Date(CSI_changes_2m$Average_Date), 
     col = "darkblue", cex.lab = 1.4,
     type = "l", lwd = 2, cex.main = 1.5, 
     xlab = "Date", ylab = "CSI", 
     main = "Changes in CSI in the CMM between 1990 and 2016")

# 2) plot number of species (richness) by average window date
plot(changes_w$Number_species ~ as.Date(changes_w$Avg_Date), 
     col = "darkred", cex.lab = 1.4, 
     type = "l", lwd = 2, cex.main = 1.5, 
     main = "Number of species observed with time", 
     xlab = "Date", ylab = "Number of species")

# 3) plot number of sampled municipalities by average window date
plot(changes_w$Number_municipalities ~ as.Date(changes_w$Avg_Date), 
     col = "darkblue", cex.lab = 1.4, 
     type = "l", lwd = 2, cex.main = 1.5, 
     main = "Number of municipalities sampled with time", 
     xlab = "Date", ylab = "Number of municipalities")

# 4) plot number of observations by average window date
plot(changes_w$Number_observations ~ as.Date(changes_w$Avg_Date), 
     col = "darkgreen", cex.lab = 1.4, 
     type = "l", lwd = 2, cex.main = 1.5,
     main = "Number of observations with time", 
     xlab = "Date", ylab = "Number of observations")
