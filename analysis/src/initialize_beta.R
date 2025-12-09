library(vegan)

args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 0){
    type <- args[1]
    occ <- args[2]
} else{
    type <- "plant" #changed to plant from pol
    occ <- "abund" 
}

if(type == "plant"){#changed to plant from pol
    speciesType <- "plant"#changed to plant from pol
} else{
    speciesType <- "pollinator" #changed from plants to pollinator
}


if(occ == "abund"){
    binary <- FALSE
    dis.method <- "chao"
    load(file=file.path('saved/communities',
                        sprintf('%s-abund.Rdata', type)))
    load(file=file.path('saved/nulls',
                        sprintf('%s-alpha.Rdata', type)))
}

if(occ == "occ"){
    occ <- "occ"
    binary <- TRUE
    dis.method <- "jaccard"
    load(file=file.path('saved/communities',
                        sprintf('%s-abund.Rdata', type)))
    load(file=file.path('saved/nulls',
                        sprintf('%s-occ.Rdata', type)))
}

if(type=="plant"){ #changed to plant from pol
    ylabel <- "Plant species turnover" #changed from Pollinator species turnover to Plant
}
