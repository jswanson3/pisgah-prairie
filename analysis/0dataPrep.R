#This script prepares data

library(tidyverse)
library(vegan)
library(fields)
library(bipartite)
setwd('C:/pisgah-prairie/data')

##******************************************************
## Clean specimen data
##******************************************************
spec <- read.csv(
  "raw/susans_data_normalized.csv",
  colClasses = "character")

dim(spec) #14211 x 11

#changes date to single format, date class, and create year column
spec$Date <- lubridate::parse_date_time(
  spec$Date,
  orders = c("d-b-y", "Ymd")
)

spec$Date <- as.Date(spec$Date)

class(spec$Date)

spec <- spec %>%
  dplyr::mutate(
    Year = year(Date))

#drop empty cells and NA
spec <-  spec[spec$Genus != '',]
spec <-  spec[spec$Species != '',]
spec <-  spec[spec$PlantGenusSpecies != '',]
spec <-  spec[!is.na(spec$Genus),]
spec <-  spec[!is.na(spec$Species),]
spec <-  spec[!is.na(spec$PlantGenusSpecies),]

# Count NAs in each column
sum(is.na(spec$Genus))
sum(is.na(spec$Species))
sum(is.na(spec$PlantGenusSpecies))

#Create genus species column
spec$GenusSpecies <- paste(spec$Genus,
                           spec$Species)

#interactions
spec$Int <- paste(spec$GenusSpecies,
                  spec$PlantGenusSpecies)

dim(spec) #5147   22

## drop non-bees
unique(spec$Family)
bee.fams <- c("Halictidae", "Andrenidae", "Apidae", "Megachilidae",
              "Colletidae", "Bombyliidae")

spec <- spec[spec$Family %in% bee.fams,]

#summary
print(paste("Bee species", length(unique(spec$GenusSpecies))))
print(paste("Plant species", length(unique(spec$PlantGenusSpecies))))
print(paste("Bee genera", length(unique(spec$Genus))))
print(paste("Interactions", length(unique(spec$Int))))
print(paste("Specimens", nrow(spec)))
print(length(unique(spec$Site)))

dim(spec)


# ## create plant by pollinator matrix to calculate specialization
# prep.comm <- aggregate(spec$GenusSpecies,
#                        by = list(PlantGenusSpecies= spec$PlantGenusSpecies,
#                                  PolGenusSpecies=spec$GenusSpecies,
#                                  Year=spec$Year,
#                                  Site=spec$Site),
#                        FUN = length)


save(spec, file='specimens/spec1.Rdata')

##******************************************************
## networks
##******************************************************
spec <- spec[!spec$PlantGenusSpecies == "",]

nets <- break.net(spec)
nets <- unlist(nets, recursive=FALSE)

save(nets, file='specimens/nets1.Rdata')

##******************************************************
## clean veg data
##******************************************************

##Create veg dataset
raw <- read_csv('C:/pisgah-prairie/data/raw/PPP_PollObs_OR_upto2022_NEW_IDs_20240108 (2).csv')
norm1 <- read_csv('C:/pisgah-prairie/data/raw/normalized1.csv')
norm2 <- read_csv("C:/pisgah-prairie/data/raw/normalized2.csv")
norm3 <- read_csv('C:/pisgah-prairie/data/raw/normalized3.csv')

norm <- rbind(norm1, norm2, norm3)

colnames(raw)

raw <- raw %>% 
  select(State, County, Latitude, Longitude, site, date, time, temp, hum, wind, plants_per_m2, patch_size)

veg <- cbind(norm, raw)

colnames(veg)

veg <- veg %>% 
  select(-occurrenceId, -acceptedUsageKey, -usageKey, -authorship, -canonicalName, -verbatimScientificName, -matchType, -confidence, -key) %>% 
  rename(Date = date, Site = site, PlantGenusSpecies = species, Genus = genus, Family = family, Kingdom = kingdom, Phylum = phylum, Class = class, Order = order, FlowerNum = plants_per_m2)

#changes date to single format, date class, and create year column
veg$Date <- lubridate::parse_date_time(
  veg$Date,
  orders = c("d-b-y", "Ymd")
)

spec$Date <- as.Date(spec$Date)

class(spec$Date)

spec <- spec %>%
  dplyr::mutate(
    Year = year(Date))

#drop empty cells and NA
veg <-  veg[veg$Genus != '',]
veg <-  veg[veg$PlantGenusSpecies != '',]
veg <-  veg[!is.na(veg$Genus),]
veg <-  veg[!is.na(veg$PlantGenusSpecies),]



## correct date format
veg$Date <- as.Date(veg$Date, format='%m/%d/%y')
veg$Year <- format(veg$Date, format='%Y')
veg$doy <- as.numeric(strftime(veg$Date, format='%j'))

veg$FlowerNum[is.na(veg$FlowerNum)] <- 0
veg$Occ <- veg$FlowerNum
veg$Occ[veg$Occ > 0] <- 1

## convert color number catagory coding to actual values
veg$logFlowerNum <- veg$FlowerNum
veg$logFlowerNum[veg$logFlowerNum == 2] <- 10
veg$logFlowerNum[veg$logFlowerNum == 3] <- 100
veg$logFlowerNum[veg$logFlowerNum == 4] <- 1000
veg$logFlowerNum[veg$logFlowerNum == 5] <- 10000

# veg$PlantGenusSpecies <-
#     fix.white.space(paste(veg$PlantGenus,
#                           veg$PlantSpecies,
#                           veg$PlantVar,
#                           veg$PlantSubSpecies))
# veg$SiteStatus <- spec$SiteStatus[match(veg$Site,
#                                         spec$Site)]
write.csv(veg, 'veg/veg.csv', row.names=FALSE)
save(veg, file='veg/veg1.Rdata')