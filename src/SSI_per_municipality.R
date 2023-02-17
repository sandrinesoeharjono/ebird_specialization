### Calculate the average SSI per municipality (taking into account the number of birds of each species,
# aka density-dependent) at 5 different 2-year windows (1994-1996, 1999-2001, 2004-2006, 2009-2011, 2014-2016) ###

# load packages and files into environment
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggthemes)
library(extrafont)
fullDatasets <- read.csv("final_shortened_fullDatasets.csv")

# remove T00:00:00Z from each entry in eventDate column to convert
fullDatasets$eventDate <- gsub("T00:00:00Z","", fullDatasets$eventDate) 

# separate fullDatasets into 5 periods (A=1994-1996, B=1999-2001, C=2004-2006, D=2009-2011, E=2014-2016)
secA <- subset(fullDatasets, eventDate >= as.Date("1994-01-01") & eventDate <= as.Date("1996-12-31"), drop=TRUE)
secB <- subset(fullDatasets, eventDate >= as.Date("1999-01-01") & eventDate <= as.Date("2001-12-31"), drop=TRUE)
secC <- subset(fullDatasets, eventDate >= as.Date("2004-01-01") & eventDate <= as.Date("2006-12-31"), drop=TRUE)
secD <- subset(fullDatasets, eventDate >= as.Date("2009-01-01") & eventDate <= as.Date("2011-12-31"), drop=TRUE)
secE <- subset(fullDatasets, eventDate >= as.Date("2014-01-01") & eventDate <= as.Date("2016-12-31"), drop=TRUE)

# add SSI column for each species (average SSI between the 3 years for each section)
# count the number of distinct municipalities for each species
# calculate the SSI based off the number of sampled municipalities in each window
secAA <- ddply(secA, ~species, summarise, distinct_municipalities=length(unique(municipalite)))
secAA$SSI <- sqrt(length(unique(unlist(secA$municipalite)))/secAA$distinct_municipalities - 1)

secBB <- ddply(secB, ~species, summarise, distinct_municipalities=length(unique(municipalite)))
secBB$SSI <- sqrt(length(unique(unlist(secB$municipalite)))/secBB$distinct_municipalities - 1)

secCC <- ddply(secC, ~species, summarise, distinct_municipalities=length(unique(municipalite)))
secCC$SSI <- sqrt(length(unique(unlist(secC$municipalite)))/secCC$distinct_municipalities - 1)

secDD <- ddply(secD, ~species, summarise, distinct_municipalities=length(unique(municipalite)))
secDD$SSI <- sqrt(length(unique(unlist(secD$municipalite)))/secDD$distinct_municipalities - 1)

secEE <- ddply(secE, ~species, summarise, distinct_municipalities=length(unique(municipalite)))
secEE$SSI <- sqrt(length(unique(unlist(secE$municipalite)))/secEE$distinct_municipalities - 1)

# merge species, municipalities and SSI for each section
secA <- merge(secA, secAA, by = "species")
secB <- merge(secB, secBB, by = "species")
secC <- merge(secC, secCC, by = "species")
secD <- merge(secD, secDD, by = "species")
secE <- merge(secE, secEE, by = "species")

# calculate average SSI per municipality (for all observations)
A_SSI_mun <- aggregate(secA$SSI ~ secA$municipalite, data = secA, 
                       FUN = function(secA) c(mean = mean(secA), count = length(secA)))
B_SSI_mun <- aggregate(secB$SSI ~ municipalite, data = secB, 
                       FUN = function(secB) c(mean = mean(secB), count = length(secB)))
C_SSI_mun <- aggregate(secC$SSI ~ municipalite, data = secC, 
                       FUN = function(secC) c(mean = mean(secC), count = length(secC)))
D_SSI_mun <- aggregate(secD$SSI ~ municipalite, data = secD, 
                       FUN = function(secD) c(mean = mean(secD), count = length(secD)))
E_SSI_mun <- aggregate(secE$SSI ~ municipalite, data = secE, 
                       FUN = function(secE) c(mean = mean(secE), count = length(secE)))

# remove unnecessary variables
rm(secAA, secBB, secCC, secDD, secEE)
rm(secA, secB, secC, secD, secE)
rm(fullDatasets) 

# change column names
setnames(A_SSI_mun, old = c('secA$municipalite','secA$SSI'), new = c('Municipality','secA.SSI'))
setnames(B_SSI_mun, old = c('municipalite','secB$SSI'), new = c('Municipality','secB.SSI'))
setnames(C_SSI_mun, old = c('municipalite','secC$SSI'), new = c('Municipality','secC.SSI'))
setnames(D_SSI_mun, old = c('municipalite','secD$SSI'), new = c('Municipality','secD.SSI'))
setnames(E_SSI_mun, old = c('municipalite','secE$SSI'), new = c('Municipality','secE.SSI'))

# save CSV files for average SSI per municipality
write.csv(A_SSI_mun, file="SSI_mun_A.csv",row.names=FALSE)
write.csv(B_SSI_mun, file="SSI_mun_B.csv",row.names=FALSE)
write.csv(C_SSI_mun, file="SSI_mun_C.csv",row.names=FALSE)
write.csv(D_SSI_mun, file="SSI_mun_D.csv",row.names=FALSE)
write.csv(E_SSI_mun, file="SSI_mun_E.csv",row.names=FALSE)

# read municipality names & types
setwd("/Users/sandrinesoeharjono/Desktop/Données/RAW DATA")
mun <- read.csv("municipality_types.csv")
mun <- dplyr::rename(mun, habitat_type = Type.d.habitat) 
mun <- dplyr::rename(mun, Municipality = Municipalité) 

# merge all files into one dataframe
comp <- full_join(mun, A_SSI_mun, by = "Municipality")
comp <- full_join(comp, B_SSI_mun, by = "Municipality")
comp <- full_join(comp, C_SSI_mun, by = "Municipality")
comp <- full_join(comp, D_SSI_mun, by = "Municipality")
comp <- full_join(comp, E_SSI_mun, by = "Municipality")

# remove unused columns
comp <- comp %>% 
  select(-"X..Logements", -"Superficie..km.2.", 
         -"Logements.hectare", -"Sur.l.île.")

# remove entries with only NAs at all 5 periods (i.e. no data)
comp <- comp %>%
  filter(Municipality != "L'Île-Dorval") %>%
  filter(Municipality != "Dollard-Des-Ormeaux") %>%
  filter(Municipality != "Mont Ste-Hilaire")

# and update habitat types for the new ones
comp[80, "habitat_type"] <- 2 # Mont Ste-Hilaire
comp[81, "habitat_type"] <- 3 # Dollars-Des-Ormeaux

# rename one municipality for compatibility with shapefile
comp[81, "Municipality"] <- "Dollard-Des-Ormeaux"

# rename columns
comp <- comp %>% rename(A = secA.SSI) 
comp <- comp %>% rename(B = secB.SSI) 
comp <- comp %>% rename(C = secC.SSI) 
comp <- comp %>% rename(D = secD.SSI) 
comp <- comp %>% rename(E = secE.SSI) 

# remove 2nd layer (SSI counts) from each period's column
comp$A <- comp$A[,-2]
comp$B <- comp$B[,-2]
comp$C <- comp$C[,-2]
comp$D <- comp$D[,-2]
comp$E <- comp$E[,-2]

# group by habitat type (1,2,3) 
SSI_df <- comp %>% 
  gather(key = key, value = value, A:E)
SSI_df$habitat_type <- as.factor(SSI_df$habitat_type)
levels(SSI_df$habitat_type) <- c("Rural", "Suburban", "Urban")

# plot average SSI for each municipality with time
SSI_graph <- ggplot(SSI_df, aes(x = key, y = value, group = Municipality, colour = habitat_type)) +
  theme_economist() +
  geom_line() +
  ggtitle("Average SSI over time") + xlab("Year") + ylab("Average SSI") + 
  theme(legend.position = "right") +
  theme(legend.title = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 8, angle = 50, hjust = 1, vjust = 1.3),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 8, hjust = 1),
        axis.line = element_blank()) +
  scale_y_continuous(limits=c(0,3)) +
  scale_color_manual(values=c("#00CCFF", "#FFCC00", "#CC0000")) +
  scale_x_discrete(labels=c("A" = "1995", "B" = "2000", "C" = "2005", "D" = "2010", "E" = "2015"))
SSI_graph + labs(color='Habitat type') 

# calculate average values per habitat group at each of the 5 periods
comp %>%
  group_by(habitat_type) %>%
  filter(!is.na(A)) %>%
  summarize(m = mean(A))
comp %>%
  group_by(habitat_type) %>%
  filter(!is.na(B)) %>%
  summarize(m = mean(B))
comp %>%
  group_by(habitat_type) %>%
  filter(!is.na(C)) %>%
  summarize(m = mean(C))
comp %>%
  group_by(habitat_type) %>%
  filter(!is.na(D)) %>%
  summarize(m = mean(D))
comp %>%
  group_by(habitat_type) %>%
  filter(!is.na(E)) %>%
  summarize(m = mean(E))
comp %>%
  filter(!is.na(A)) %>%
  summarize(m = mean(A))
comp %>%
  filter(!is.na(B)) %>%
  summarize(m = mean(B))
comp %>%
  filter(!is.na(C)) %>%
  summarize(m = mean(C))
comp %>%
  filter(!is.na(D)) %>%
  summarize(m = mean(D))
comp %>%
  filter(!is.na(E)) %>%
  summarize(m = mean(E))
