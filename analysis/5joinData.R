##This script joins beta dispersion, species roles, and delta abundance dataframes
library(tidyverse)
setwd("C:/pisgah-prairie/analysis")
load('../saved/results/specRoles.Rdata')
load('../saved/results/deltaAbund.Rdata')
load('../saved/results/partnerVar.Rdata')
load('../saved/results/pcaVar.Rdata')

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

spec.roles.tidy <- species.roles %>% 
  dplyr::rename(PlantGenusSpecies = GenusSpecies) %>% 
  dplyr::mutate(
    PlantGenusSpecies = as.character(PlantGenusSpecies),
    Year = as.character(Year), 
    Site = as.character(Site),
    SpSiteYear = paste0(PlantGenusSpecies, Site, Year, " "),
    SpSiteYear = trimws(SpSiteYear))

pca.scores.tidy <- plant.pca.scores.df %>% 
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
str(spec.roles.tidy$SpSiteYear)

head(delta.tidy$SpSiteYear, n = 10)
head(beta.dist.tidy$SpSiteYear, n = 10)
head(spec.roles.tidy$SpSiteYear, n = 10)

#Join dataframes
specAnalysis <- spec.roles.tidy %>%
  dplyr::left_join(delta.tidy %>% select(-Year, -Site, -PlantGenusSpecies), by = "SpSiteYear") %>%
  dplyr::left_join(beta.dist.tidy %>% select(-Year, -Site, -Date, -PlantGenusSpecies), by = "SpSiteYear") %>% 
  dplyr::left_join(pca.scores.tidy %>% select(-Year, -Site, -PlantGenusSpecies), by = "SpSiteYear")

# Filter data for non-zero deltaAbund and beta dispersion
specAnalysis <- specAnalysis %>% 
  dplyr::filter(dist != 0, deltaAbund != 0, mean.pca1 !=0) %>% 
  na.omit()

#visualize data of interest
hist(specAnalysis$dist)
hist(specAnalysis$deltaAbund)
hist(specAnalysis$d)
hist(specAnalysis$weighted.closeness)
hist(specAnalysis$mean.pca1)

save(specAnalysis, file="saved/results/specAnalysis.Rdata")

