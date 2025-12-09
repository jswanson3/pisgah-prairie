#This script adds the packages used to the library
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

suppressWarnings(
  suppressPackageStartupMessages({
    library(tidyverse)
    library(vegan)
    library(fields)
    library(bipartite)
    library(lme4)
    library(lmerTest)
    library(fossil)
    library(ggfortify)
    library(cowplot)
    library(vegan)
  })
)