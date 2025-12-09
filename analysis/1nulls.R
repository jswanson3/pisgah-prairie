## This script generates randomized "communities" for standardizied
## partner beta diversity scores. Because species will vry in their
## partner richness, this standardized for partner "alpha diversity"
## for the beta diversity calculation
rm(list=ls())
library(bipartite, quietly = TRUE)
library(lme4, quietly = TRUE)
library(lmerTest, quietly = TRUE)

setwd('C:/pisgah-prairie/analysis')
this.script <- "nulls"
source('src/misc.R')
source('src/calcPca.R')
source('src/calcSpec.R')
source('src/commPrep.R')
source('src/vaznull2.R')

load('../data/specimens/spec1.Rdata')
load('../data/specimens/nets1.Rdata')
save.path <- 'saved'

save.dir.comm <- "saved/communities"
save.dir.nulls <- "saved/nulls"

type <- "plant"
nnull <- 9 #why such a low null?
species.type="PlantGenusSpecies" #changed from bee (GenusSpecies) to plant (PlantGenusSpecies)
species.type.int="GenusSpecies" #changed from plant (PlantGenusSpecies) to bee (GenusSpecies)


sites <- unique(spec$Site)
years <- unique(spec$Year)

## ************************************************************
## year by species matrices plants
## ************************************************************
comms <- lapply(years, calcSiteBeta,
                species.type=species.type,
                spec=spec,
                species.type.int=species.type.int)

comm <- makePretty(comms, spec)

save(comm, file=file.path(save.dir.comm,
                          sprintf('%s-abund.Rdata', type)))

## ************************************************************
## alpha div nulls
## ************************************************************
nulls <- rapply(comm$comm, vaznull.2, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-alpha.Rdata', type)))

## ************************************************************
## occurrence nulls
## ************************************************************
occ.null <- function(web){
    simulate(vegan::nullmodel(web, method="quasiswap"),1)[,,1]
}

rep.occ.null <- function(web, N){
    replicate(N, occ.null(web), simplify = FALSE)
}

nulls <- rapply(comm$comm, rep.occ.null, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-occ.Rdata', type)))

## ************************************************************
## test of variability metrics
## ************************************************************
## set interactions to be all the same across sampling rounds for each
## species
test.comm <- lapply(comm$comm$'2019', function(x){ 
    x[1:nrow(x), 1:ncol(x)] <- 0
    x[,c(1,2,3)] <- 2
    x
})
comm$comm$'2019' <- test.comm
nulls <- rapply(comm$comm, vaznull.2, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-alpha-test.Rdata', type)))
save(comm, file=file.path(save.dir.comm,
                          sprintf('%s-abund-test.Rdata', type)))
