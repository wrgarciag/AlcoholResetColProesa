# This script process ENPH 2017 files. Output is a data table for 
# SSB QUAIDS estimation

rm(list=ls())

###.....................................................................
### Macros

## Libraries

library(data.table)
library(foreign)
library(readstata13)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(survey)
#install.packages("extrafont")
library(extrafont)
library(dplyr)
library(magrittr)
library(tidyr)
library(fixest)
library(fastDummies)
library(hot.deck)
library(mice)
library(haven)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()    


## Working directory

# Main folders
#wd      <- "C:/Users/William/OneDrive - ICESI/2Publicaciones/SSBElasticity/"
#wd      <- "D:/Usuarios/1018428549/OneDrive - ICESI/2Publicaciones/SSBElasticity/"
#wd <- "G:/Mi unidad/02Work/7ICESI/1PROESA/2Publicaciones/SSBElasticity/"

wd <- "C:/Users/William/Documents/SSBElasticity/"
wd <- "C:/Users/wrgar/Universidad Icesi (@icesi.edu.co)/Proesa - 22-1002-PahoElasSsb/"
#wd <- "C:/Users/William/Universidad Icesi (@icesi.edu.co)/Proesa - 22-1002-PahoElasSsb/"

wd <- "C:/Users/wgarcia/Universidad Icesi (@icesi.edu.co)/Proesa - 22-1002-PahoElasSsb/"

wd <- "H:/My Drive/Research/Proesa - 22-1002-PahoElasSsb/"

# Nm
#wd      <- "D:/usuarios/80088802/Universidad Icesi/William Ricardo Garcia Garcia - SSBElasticity/"

wd_code <- paste0(wd,"Code/")
wd_data <- paste0(wd,"Data/")
wd_resu <- paste0(wd,"Resu/")

# Subfolders
wdd_in  <- paste0(wd_data,"Input/")
wdd_out <- paste0(wd_data,"Output/")
wdr_gra <- paste0(wd_resu,"Graf/")

setwd(wd_code)

### Parameters

prural  <- TRUE
psample <- FALSE
pstata  <- FALSE

# Price/Quantities
v.precio <- 2258
# SSB
pqlitro    <- 1
pqlitSSB   <- 1
pglitSSB   <- 2500

# Milk/Derivatives
pqlitMD   <- 0.5
# HotDrinks
pqlitHot   <- 0.35

# Beer
pqlitBeer   <- 1
pglitBeer   <- 5000

# Water
pqlitWa  <- 1
pglitWa  <- 1000

# Spirits
pqlitSpi  <- 1
pglitSpi  <- 10000

# Wine
pqlitWi   <- 1
pglitWi   <- 28000


##.....................................................................
## function to classify items
source("fclasicoicop.R")

##.....................................................................
## Input data sets
source("SSBDInput.R")

#source("SSBEBootstrap.R")

##.....................................................................
## Household data set
source("SSBDHousehold.R")

##.....................................................................
## Income data set
source("SSBDIncome.R")

# ##.....................................................................
# ## Expenditure dataset
# source("SSBDExpenditure.R")
source("SSBEConsuAna.R")

# ##.....................................................................
# ## Implicit Prices dataset
# source("SSBDPrices.R")

source("SSBDEPrices.R")


##.....................................................................
## Revision Paper

source("SSBEConsuBasesUrbano.R")
source("SSBEConsuBasesRural.R")

source("SSBConsuGasCorrected.R")
source("SSBDImputacion.R")


source("SSBRevisedPaper_Prices.R")
source("SSBRevisedPaper_Prices_Codi.R")


## Guarda la base de datos para las estimacione

#Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))
saveRDS(Data,file=paste0(wdd_out,"DataEstimates.rds"))
save.dta13(Data,file=paste0(wdd_out,"DataEstimates.dta"))

















# ##.....................................................................
# ## Descriptive extended paper
# source("SSBEEPaper.R")
# 
# ##.....................................................................
# ## Descriptive extended paper
# source("SSBEEPaper.R")
# 
# ##.....................................................................
# ## Descriptive statistics
# source("SSBEConsumption.R")
# 
# 
# ##.....................................................................
# ## Descriptive statistics
# source("SSBEDescriptive.R")
# 
# ##.....................................................................
# ## Descriptive statistics
# source("SSBEDescribe.R")
# 
# ##.....................................................................
# ## Implicit Prices dataset
# source("SSBRPlots.R")
# 
# 
# gc()
# detach(package:data.table)
# 

