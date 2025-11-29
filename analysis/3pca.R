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
species.roles.tidy <- species.roles[species.roles$speciesType == "plant",]

save(species.roles.tidy, file="saved/results/specRoles.Rdata")
