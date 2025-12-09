##This script fits models to data and creates plots
rm(list=ls())
setwd('C:/pisgah-prairie/analysis')
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyverse)
library(cowplot)
load('saved/results/specAnalysis.Rdata')
source('src/predictIntervals.R')

## visualizes and removes outliers.
## The outliers have no effect on models and is more visually appealing when removed
hist(specAnalysis$deltaAbund)
hist(specAnalysis$d)
hist(specAnalysis$var.pca1)
hist(specAnalysis$deltaAbund)
hist(specAnalysis$nestedrank)
sum(is.na(specAnalysis$deltaAbund))

#Create linear mixed models
##############
##Delta floral abundance vs partner variability
##############
delta.abund.mod <- lmerTest::lmer(deltaAbund ~ scale(dist) + (1|Site),  data = specAnalysis)
summary(delta.abund.mod)

##############
##d vs partner variability
##############
d.mod <- lmerTest::lmer(d ~ scale(dist) + (1|Site), data = specAnalysis)
summary(d.mod)

##############
##Weighted closeness vs partner variability
##############
close.mod <- lmerTest::lmer(weighted.closeness ~ scale(dist) + (1|Site), data = specAnalysis)
summary(close.mod)

##############
##Nestedness vs partner variability
##############
nested.mod <- lmerTest::lmer(nestedrank ~ scale(dist) + (1|Site), data = specAnalysis)
summary(nested.mod)

##############
##role variability vs partner variability
##############
#identify extreme outliers and remove (-300+ variance)
which(specAnalysis$var.pca1 < -100)
specAnalysis <- specAnalysis[-c(135, 136, 137, 138, 144, 145, 146, 147), ] 
pca.mod <- lmerTest::lmer(var.pca1 ~ scale(dist) + (1|Site), data = specAnalysis)
summary(pca.mod)


#This function uses a linear model to predict values for a network metric 
#and creates plots to visualize
plot.s <- function(specAnalysis, response, mod) {
  # Create prediction grid
  expand.grid <- data.frame(
    dist = seq(min(specAnalysis$dist),
               max(specAnalysis$dist),
               length.out = 100)
  )
  
  # Add placeholder column for response
  expand.grid[[response]] <- 0
  
  # Predict using custom function
  pred <- predict.int(mod = mod,
                      dd = expand.grid,
                      y = response,
                      family = "gaussian")
  
  # Plot
  ggplot() +
    # geom_point(data = pred, aes(x = dist, y = .data[[response]])) +
    geom_line(data = pred, aes(x = dist, y = .data[[response]]), color = "blue", linewidth = 1.2) +
    geom_ribbon(data = pred, aes(x = dist, ymin = plo, ymax = phi), alpha = 0.2, fill = "blue") +
    theme_classic() +
    theme(text = element_text(size = 20))
}

plot.ns <- function(specAnalysis, response, mod) {
  # Create prediction grid
  expand.grid <- data.frame(
    dist = seq(min(specAnalysis$dist),
               max(specAnalysis$dist),
               length.out = 100)
  )
  
  # Add placeholder column for response
  expand.grid[[response]] <- 0
  
  # Predict using custom function
  pred <- predict.int(mod = mod,
                      dd = expand.grid,
                      y = response,
                      family = "gaussian")
  
  # Plot
  ggplot() +
    # geom_point(data = pred, aes(x = dist, y = .data[[response]])) +
    geom_line(data = pred, aes(x = dist, y = .data[[response]]), color = "blue", linetype = "dashed", linewidth = 1.2) +
    geom_ribbon(data = pred, aes(x = dist, ymin = plo, ymax = phi), alpha = 0.2, fill = "blue") +
    theme_classic() +
    theme(text = element_text(size = 20))
}

##Plot models
#########################
##Delta abundance vs partner variability
#########################
delta.abund.plot <- plot.ns(specAnalysis, "deltaAbund", delta.abund.mod)

delta.abund.plot.labeled <- delta.abund.plot + labs(x = "Partner Variability", y = "Log-ratio Floral Abundance")

#########################
##d vs partner variability
#########################
d.dist.plot <- plot.ns(specAnalysis, "d", d.mod)

d.dist.plot.labeled <- d.dist.plot + labs(x = "Partner Variability", y = "d")

#########################
##Nestedness vs partner variability
#########################
nested.dist.plot <- plot.ns(specAnalysis, "nestedrank", nested.mod)

nested.dist.plot.labeled <- nested.dist.plot + labs(x = "Partner Variability", y = "Nested Rank")

#########################
##Role variability vs partner variability
#########################
pca.dist.plot <- plot.s(specAnalysis, "var.pca1", pca.mod)

pca.dist.plot.labeled <- pca.dist.plot + labs(x = "Partner Variability", y = "Network Niche Variability")

#Combine plots into single grid
combinedPlot <- cowplot::plot_grid(delta.abund.plot.labeled, pca.dist.plot.labeled, d.dist.plot.labeled,
                                   nested.dist.plot.labeled, ncol = 2)
print(combinedPlot)

# Save plot
ggplot2::ggsave(filename = "ModbetaDispNetMetricPlot.png",
                plot = combinedPlot,
                path = "figures",      
                width = 10, height = 8, dpi = 300)
