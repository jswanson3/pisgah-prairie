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
which(specAnalysis$deltaAbund > 2)
specAnalysis[541, ]
specAnalysis[1106, ]
specAnalysis <- specAnalysis[-c(541, 1106), ]

#Create linear mixed models
##############
##Delta floral abundance vs partner variability
##############
delta.abund.mod <- lmerTest::lmer(deltaAbund ~ scale(dist) + (1|Site), data = specAnalysis)

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


#This function uses a linear model to predict values for a network metric and then visualize
plot <- function(specAnalysis, response, mod) {
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
    geom_point(data = specAnalysis, aes(x = dist, y = .data[[response]])) +
    geom_line(data = pred, aes(x = dist, y = .data[[response]]), color = "blue", linewidth = 1.2) +
    geom_ribbon(data = pred, aes(x = dist, ymin = plo, ymax = phi), alpha = 0.2, fill = "blue") +
    theme_minimal()
}

##Plot models
#########################
##Delta abundance vs beta dispersion
#########################
delta.abund.plot <- plot(specAnalysis, "deltaAbund", delta.abund.mod)

delta.abund.plot.labeled <- delta.abund.plot + labs(x = "Partner Variability", y = "Delta Floral Abundance")

#########################
##d vs beta dispersion
#########################
d.dist.plot <- plot(specAnalysis, "d", d.mod)

d.dist.plot.labeled <- d.dist.plot + labs(x = "Partner Variability", y = "d")

#########################
##Weighted closeness vs beta dispersion
#########################
close.dist.plot <- plot(specAnalysis, "weighted.closeness", close.mod)

close.dist.plot.labeled <- close.dist.plot + labs(x = "Partner Variability", y = "Weighted Closeness")

#########################
##Nestedness vs beta dispersion
#########################
nested.dist.plot <- plot(specAnalysis, "nestedrank", nested.mod)

nested.dist.plot.labeled <- nested.dist.plot + labs(x = "Partner Variability", y = "Nestedness")


#Combines plots into single grid
combinedPlot <- cowplot::plot_grid(delta.abund.plot.labeled, d.dist.plot.labeled,
                                   close.dist.plot.labeled, nested.dist.plot.labeled,
                                   labels = c("A", "B", "C", "D"),
                                   ncol = 2)
print(combinedPlot)



# Save plot
ggplot2::ggsave(filename = "betaDispNetMetricPlot.png",
                plot = combinedPlot,
                path = "figures",      
                width = 10, height = 8, dpi = 300)
