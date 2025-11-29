## This script calculates the log abundance of each species at each site

rm(list=ls())
setwd('analysis/variability')
library(lme4, quietly = TRUE)
library(lmerTest, quietly = TRUE)
source('src/setup.R')
source('src/delta.R')
source('src/misc.R')
load('../data/specimens/spec1.Rdata')
load('../data/veg/veg1.Rdata')

## Calculate the abundance of each species in each year
plant.abund <- calcPlantABund(spec) 

## calculate the difference the log ratio of abundance between year pairs
plant.delta <- calcPlantYearDiff(plant.abund)

#converts list to dataframe
plant.delta.df <- bind_rows(plant.delta , .id = "Year")

plant.delta.tidy <- plant.delta.df%>% 
  rename(PlantGenusSpecies = GenusSpecies) %>% 
  mutate(Site = as.character(Site),
         Year = as.character(Year),
         PlantGenusSpecies = as.character(PlantGenusSpecies))

save(plant.delta.tidy, file="saved/results/deltaAbund.Rdata")
