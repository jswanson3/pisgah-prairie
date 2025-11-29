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

# mean.beta.dist <- tapply(beta.dist$dist[beta.dist$Year == "2019"],
#                          beta.dist$GenusSpecies[beta.dist$Year == "2019"],
#                          cv)
# 
# ## super small numbers, as expected
# hist(mean.beta.dist)

var.beta.dist <- tapply(beta.dist$dist[beta.dist$Year == "2019"],
                        beta.dist$PlantGenusSpecies[beta.dist$Year == "2019"],
                        var.method)

save(beta.dist, file="saved/results/partnerVar.Rdata")
