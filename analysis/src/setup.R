zscore <- function(x){
    y <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
    return(y)
}

calcPlantABund <- function(spec){
    ## format spec data and subset to net data
    spec$Doy <- as.numeric(strftime(spec$Date, format = "%j"))
    # spec <- spec[spec$NetPan =="net",]

    ## calculate abundances for each species at each sample data
    plant.abund <- aggregate(list(PlantAbund=spec$PlantGenusSpecies), #changed from GenusSpecies(bees) to PlantGenusSpecies(plants)
                            list(PlantGenusSpecies=spec$PlantGenusSpecies, #changed from GenusSpecies(bees) to PlantGenusSpecies(plants)
                                 Site=spec$Site,
                                 Year=spec$Year,
                                 Date=spec$Date),
                                 # SiteStatus=spec$SiteStatus),
                            length)

    # spec.abund$SiteStatus <- factor(spec.abund$SiteStatus,
    #                                 levels=c("LOW", "MOD", "HIGH"))
    return(plant.abund)
}

calcSpecABund <- function(spec){
  ## format spec data and subset to net data
  spec$Doy <- as.numeric(strftime(spec$Date, format = "%j"))
  # spec <- spec[spec$NetPan =="net",]
  
  ## calculate abundances for each species at each sample data
  spec.abund <- aggregate(list(specAbund=spec$GenusSpecies), #changed from GenusSpecies(bees) to PlantGenusSpecies(plants)
                                list(GenusSpecies=spec$GenusSpecies, #changed from GenusSpecies(bees) to PlantGenusSpecies(plants)
                                     Site=spec$Site,
                                     Year=spec$Year,
                                     Date=spec$Date),
                                # SiteStatus=spec$SiteStatus),
                                length)
  
  # spec.abund$SiteStatus <- factor(spec.abund$SiteStatus,
  #                                 levels=c("LOW", "MOD", "HIGH"))
  return(spec.abund)
}


getScorePrepDrought <- function(plant.pca.scores,
                                var.beta.dist,
                                spec.abund){
    ## selects pre drought pca and beta dist scores (from 2019) and
    ## add to data from the objects outputted by 2partner.R and
    ## 3role.R.
    pcas.2019 <- plant.pca.scores$'2019'$pcas
    spec.abund <- merge(spec.abund, pcas.2019, all.x=TRUE)

    ## add partner varaibility data from 2019
    spec.abund$beta.dist <- var.beta.dist[match(spec.abund$PlantGenusSpecies, #Changed from GenusSpecies (bees) to PlantGenusSpecies (plants)
                                                 names(var.beta.dist))]
    return(spec.abund)
}
