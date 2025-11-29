##This script joins beta dispersion, species roles, and delta abundance dataframes

library(tidyverse)
load('../saved/results/specRoles.Rdata')
load('../saved/deltaAbund.Rdata')
load('../saved/partnerVar.Rdata')

#Create key SpSiteYear and change class to character for each dataframe
delta.tidy <- plant.delta.tidy %>% 
  dplyr::mutate(
    PlantGenusSpecies = as.character(PlantGenusSpecies),
    Year = as.character(Year), 
    Site = as.character(Site),
    SpSiteYear = paste0(PlantGenusSpecies, Site, Year, " "),
    SpSiteYear = trimws(SpSiteYear))

beta.dist.tidy <- beta.dist %>%
  dplyr::mutate(
    PlantGenusSpecies = as.character(PlantGenusSpecies),
    Year = as.character(Year), 
    Site = as.character(Site),
    SpSiteYear = paste0(PlantGenusSpecies, Site, Year, " "),
    SpSiteYear = trimws(SpSiteYear))

spec.roles.tidy <- species.roles.tidy %>% 
  dplyr::rename(PlantGenusSpecies = GenusSpecies) %>% 
  dplyr::mutate(
    PlantGenusSpecies = as.character(PlantGenusSpecies),
    Year = as.character(Year), 
    Site = as.character(Site),
    SpSiteYear = paste0(PlantGenusSpecies, Site, Year, " "),
    SpSiteYear = trimws(SpSiteYear))

#Check structure of SpSiteYear for consistency
str(delta.tidy$SpSiteYear)
str(beta.dist.tidy$SpSiteYear)
str(species.roles.tidy$SpSiteYear)

head(delta.tidy$SpSiteYear, n = 10)
head(beta.dist.tidy$SpSiteYear, n = 10)
head(spec.roles.tidy$SpSiteYear, n = 10)

#Join dataframes
specAnalysis <- spec.roles.tidy %>%
  dplyr::left_join(delta.tidy %>% select(-Year, -Site, -PlantGenusSpecies), by = "SpSiteYear") %>%
  dplyr::left_join(beta.dist.tidy %>% select(-Year, -Site, -Date, -PlantGenusSpecies), by = "SpSiteYear")

# Filter data for non-zero deltaAbund and beta dispersion
specAnalysis <- specAnalysis %>% 
  dplyr::filter(dist != 0, deltaAbund != 0) %>% 
  na.omit()

#visualize data of interest
hist(specAnalysis$dist)
hist(specAnalysis$deltaAbund)
hist(specAnalysis$d)
hist(specAnalysis$weighted.closeness)

save(specAnalysis, file="saved/results/specAnalysis.Rdata")

