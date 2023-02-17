### Map bird observations from different periods (for Appendix A1) ###

# load packages and files into environment
library(rworldmap)
library(dplyr)
Data1 <- read.csv(file="1990-1995.csv", header=TRUE, sep="\t")
Data2014 <- read.csv(file="2014.csv", header=TRUE, sep="\t")
Data2015 <- read.csv(file="2015.csv", header=TRUE, sep="\t")
Comparison <- rbind(Data2014, Data2015)
rm(Data2014, Data2015)

# plot birds of Turdus migratorius species for summer 2014 & winter 2015
summer1 <- Comparison %>%
  filter(species=="Turdus migratorius") %>%
  filter(as.Date(eventDate) > "2014-06-21" & as.Date(eventDate) < "2014-09-22")
winter1 <- Comparison %>%
  filter(species=="Turdus migratorius") %>%
  filter(as.Date(eventDate) > "2014-12-21" & as.Date(eventDate) < "2015-03-20")

# plot map with coordinates in range of data points
AppendixA1a <- getMap(resolution="high")
plot(AppendixA1a, 
     xlim = range(Data1$decimalLongitude), 
     ylim = range(Data1$decimalLatitude), 
     main = "Summer 2014",
     col="lightgrey", bg="white", asp = 1) +
  points(summer1$decimalLongitude, summer1$decimalLatitude, col="black", cex = 1, pch = 17)
AppendixA1a <- recordPlot()

AppendixA1b <- getMap(resolution="high")
plot(AppendixA1b, 
     xlim = range(Data1$decimalLongitude), 
     ylim = range(Data1$decimalLatitude), 
     main = "Winter 2014-2015",
     col="lightgrey", bg="white", asp = 1) +
  points(winter1$decimalLongitude, winter1$decimalLatitude, col="black", cex = 1, pch = 17)
AppendixA1b <- recordPlot()
  
# plot birds of Mergus serrator species for summer 2014 & winter 2015 - Appendix A2
summer2 <- Comparison %>%
  filter(species=="Mergus serrator") %>%
  filter(as.Date(eventDate) > "2014-06-21" & as.Date(eventDate) < "2014-09-22")
winter2 <- Comparison %>%
  filter(species=="Mergus serrator") %>%
  filter(as.Date(eventDate) > "2014-12-21" & as.Date(eventDate) < "2015-03-20")

# plot map with coordinates in range of data points
AppendixA2a <- getMap(resolution="high")
plot(AppendixA2a, 
     xlim = range(Data1$decimalLongitude), 
     ylim = range(Data1$decimalLatitude), 
     main = "Summer 2014",
     col="lightgrey", bg="white", asp = 1) +
  points(summer2$decimalLongitude, summer2$decimalLatitude, col="black", cex = 1, pch = 17)
AppendixA2a <- recordPlot()

AppendixA2b <- getMap(resolution="high")
plot(AppendixA2b, 
     xlim = range(Data1$decimalLongitude), 
     ylim = range(Data1$decimalLatitude), 
     main = "Winter 2014-2015",
     col="lightgrey", bg="white", asp = 1) +
  points(winter2$decimalLongitude, winter2$decimalLatitude, col="black", cex = 1, pch = 17)
AppendixA2b <- recordPlot()

# plot birds of Bombycilla cedrorum species for summer 2014 & winter 2015 - Appendix A3
summer3 <- Comparison %>%
  filter(species=="Bombycilla cedrorum") %>%
  filter(as.Date(eventDate) > "2014-06-21" & as.Date(eventDate) < "2014-09-22")
winter3 <- Comparison %>%
  filter(species=="Bombycilla cedrorum") %>%
  filter(as.Date(eventDate) > "2014-12-21" & as.Date(eventDate) < "2015-03-20")

# plot map with coordinates in range of data points
AppendixA3a <- getMap(resolution="high")
plot(AppendixA3a, 
     xlim = range(Data1$decimalLongitude), 
     ylim = range(Data1$decimalLatitude), 
     main = "Summer 2014",
     col="lightgrey", bg="white", asp = 1) +
  points(summer3$decimalLongitude, summer3$decimalLatitude, col="black", cex = 1, pch = 17)
AppendixA3a <- recordPlot()

AppendixA3b <- getMap(resolution="high")
plot(AppendixA3b, 
     xlim = range(Data1$decimalLongitude), 
     ylim = range(Data1$decimalLatitude), 
     main = "Winter 2014-2015",
     col="lightgrey", bg="white", asp = 1) +
  points(winter3$decimalLongitude, winter3$decimalLatitude, col="black", cex = 1, pch = 17)
AppendixA3b <- recordPlot()
