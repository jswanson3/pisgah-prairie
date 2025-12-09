## This script calculates plant network roles
rm(list=ls())
library(ggfortify)
library(bipartite)
setwd('analysis/variability')
source('src/misc.R')
source('src/calcPca.R')
source('src/calcSpec.R')
source('src/commPrep.R')
source('src/vaznull2.R')

load('../data/specimens/spec1.Rdata')
load('../data/specimens/nets1.Rdata')
this.script <- "role"
type <- "all"

## applies specieslevel from bipartite to networks
## calculates the species roles from a network and returns a dataframe 
species.roles <- calcSpec(nets, spec, dist.metric="chao")
species.roles <- species.roles[species.roles$speciesType == "plant",]

save(species.roles, file="saved/results/specRoles.Rdata")

species.roles

## vector of pca loadings of interest
loadings <- c(1)
metrics <- c("rare.degree",
             "weighted.betweenness",
             "weighted.closeness",
             "niche.overlap",
             "species.strength",
             "d")

## the metrics used in the PCA
var.method <- cv
ave.method <- mean

#changed from pollinator to plant
plant.pca.scores <- calcPcaMeanVar(species.roles=species.roles, 
                                   var.method=var.method,
                                   ave.method=ave.method,
                                   metrics= metrics,
                                   loadings=loadings,
                                   agg.col = "Year")

autoplot(plant.pca.scores$'2019'$pca.loadings, loadings=TRUE,
         loadings.colour = 'blue')


plant.pca.scores[1]

plant.pca.scores.df <- do.call(rbind, lapply(plant.pca.scores, function(x) x$pcas))


save(plant.pca.scores.df,  file="saved/results/pcaVar.Rdata")
