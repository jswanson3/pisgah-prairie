## This script calculates the "beta diversity" of interaction partners
## for each species. For this study, the partner beta diversity is
## calculated across sites within a year
rm(list=ls())
setwd('analysis/variability')
source('src/initialize_beta.R')
source('src/misc.R')
source('src/calcPca.R')
source('src/calcSpec.R')
source('src/beta.R')
library(ggplot2)
library(tidyverse)

## metrics for taking the average and variability of beta div
var.method <- cv

dis <- mapply(function(a, b, c, d)
    calcBeta(comm= a, ## observed communities
             dis.method, ## dissimilarity metric
             nulls=b, ## null communities
             occ=binary, ## binary or abundance weighted?
             sub=type,
             zscore=FALSE), ## use zscores vs. Chase method?
    a=comm$comm,
    b= nulls,
    SIMPLIFY=FALSE)

beta.dist <- makeBetaDataPretty() 


save(beta.dist, file="saved/results/partnerVar.Rdata")

# 1. Compute mean for each site
site_means <- beta.dist %>%
  group_by(Site) %>%
  summarise(mean_dist = mean(dist, na.rm = TRUE))

#visualize beta diversity
partHistPlot <- ggplot2::ggplot(beta.dist, aes(x = dist)) +
  geom_histogram(fill = "blue", color = "white") +
  geom_vline(data = site_means,
             aes(xintercept = mean_dist),
             color = "red", linewidth = 1.2) +
  labs(x = "Partner Variability", y = "Frequency") +
  theme_classic() +
  theme(text = element_text(size = 30)) +
  facet_wrap(~Site, ncol = 3)

# Save plot
ggplot2::ggsave(filename = "parVarHistPlot.png",
                plot = partHistPlot,
                path = "figures",
                width = 14, height = 8, dpi = 300)
 

# #This function takes the argument years and beta.dis and shuffles the 
# #PlantGenusSpecies column using sample, adds the shuffled column back on 
# #to the table, and then takes the mean of the simulated beta diversity
# randomizeTraits <- function(yr, data) {
#   # Filter to the chosen year
#   yeardata <- dplyr::filter(data, Year == yr)
#   
#   # Shuffle site status within that year only
#   year.shuffle <- sample(yeardata$PlantGenusSpecies, nrow(yeardata), replace = FALSE)
#   yeardata$PlantGenusSpecies <- year.shuffle
#   
#   # Summarise for this year
#   yeardata %>%
#     dplyr::group_by(Year, PlantGenusSpecies) %>%
#     dplyr::summarise(
#       MeanBeta = mean(dist, na.rm = TRUE),
#       .groups = "drop"
#     )
# }
# 
# #test function
# randomizeTraits(2019, beta.dist) 
# 
# #create vector of unique data
# years <- sort(unique(beta.dist$Year))
# 
# ###This function shuffles PlantGenusSpecies using the randomize traits function, 
# #loops over the remaining years, binds the data by row to prior years, 
# #and calculates the mean of beta diversity across years, and returns that value. 
# repRandomComm <- function(beta.dist, years) {
#   random.traits <- randomizeTraits(years[1], beta.dist)
#   
#   for (i in 2:length(years)) { 
#     yrnew.year <- randomizeTraits(years[i], beta.dist)
#     random.traits <- rbind(random.traits, yrnew.year)
#   }
#   mean.years <- random.traits %>% 
#     dplyr::group_by(PlantGenusSpecies) %>% 
#     dplyr:summarize(
#       MeanBeta = mean(MeanBeta, na.rm = TRUE),
#       .groups = "drop")
# }
# 
# #calculates null beta diversity across years
# null.mean <- repRandomComm(beta.dist, years)
# 
# niter <- 100
# 
# #creates randomized community
# randomized.comms <- repRandomComm(beta.dist, years)
# 
# #100 permutations
# for(i in 1:(niter-1)) {
#   new.comm.means <- repRandomComm(beta.dist, years)
#   randomized.comms <- rbind(randomized.comms, new.comm.means)
# }
# 
# #calculate observed statistic
# obs.means.yr <- beta.dist %>%
#   dplyr::group_by(Year, Site, PlantGenusSpecies) %>%
#   dplyr::summarise(MeanBeta = mean(dist))
# 
# obs.means <- obs.means.yr %>%
#   dplyr::group_by(PlantGenusSpecies) %>%
#   dplyr::summarise(MeanBeta = mean(MeanBeta))
# 
# #visualize random and observed
# null.mean$Source <- "Null"
# obs.means$Source <- "Observed"
# 
# combined_data <- rbind(null.mean, obs.means)
# 
# null.obs.hist <- ggplot2::ggplot(combined_data, aes(x = MeanBeta, fill = Source)) +
#   geom_histogram(alpha = 1, position = "nudge", color = "white") +
#   scale_fill_manual(values = c("Null" = "gray25", "Observed" = "blue")) +
#   labs(x = "Beta Diversity", y = "Frequency", fill = "Legend") +
#   theme_classic() +
#   theme(text = element_text(size = 25))
# 
# # Save plot
# ggplot2::ggsave(filename = "nullObsHist.png",
#                 plot = null.obs.hist,
#                 path = "figures",      
#                 width = 14, height = 8, dpi = 300)
# 
# #Test null hypothesis
# sum(null.mean$MeanBeta <= obs.means$MeanBeta)/nrow(null.mean)
#     