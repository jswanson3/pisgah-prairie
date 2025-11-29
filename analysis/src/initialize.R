library(bipartite, quietly = TRUE)
library(lme4, quietly = TRUE)
library(lmerTest, quietly = TRUE)
source('src/misc.R')
source('src/calcPca.R')
source('src/calcSpec.R')

load('../data/specimens/spec1.Rdata')
load('../data/specimens/nets1.Rdata')
save.path <- 'saved'

save.dir.comm <- "saved/communities"
save.dir.nulls <- "saved/nulls"

type <- "plant"
nnull <- 9
species.type="PlantGenusSpecies" #changed from specimen (GenusSpecies)
species.type.int="GenusSpecies" #changed from plant (PlantGenusSpecies)


