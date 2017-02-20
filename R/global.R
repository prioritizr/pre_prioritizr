library(shiny)
library(shinyIncubator)
library(ggplot2)
library(sp)
library(rgdal)
library(maptools)
library(PBSmapping)
library(foreign)
library(sqldf)
library(vegan)
library(labdsv)
library(raster)
library(leaflet)
library(rhandsontable)
library(Matrix)
library(plyr)
library(dplyr)
library(tidyr)
library(gurobi)

### Source R scripts
## don't source in packages
source("helper.functions.R")
source("fit.gurobi.R")
#source("marxan.R")

### Load Workspaces
pu <- read.csv("./data/pu.csv")
puvsf <- read.csv("./data/puvsf.csv")
feat <- read.csv("./data/feat.csv")
#bound <- read.csv("./data/bound.csv")

### Load rasters
in.raster <- raster(paste0(getwd(),"/data/idx.tif"))
in.rast.val <- getValues(in.raster)
idx.r <- raster(paste0(getwd(),"/data/idx.tif"))
idx.r.val <- getValues(idx.r)


#Setup "Edit Target" table
feat.lst <- data.frame(id=seq(1,ncol(puvsf[,-1])),
                       Percent=17,
                       name=names(puvsf[,-1]),
                       stringsAsFactors =F)
l.feat.lst <- nrow(feat.lst)


#setup "Edit Scenarios" table
scen <- data.frame(scenario="template",
#                   time="curr",
                   cost="area",
#                   protected="locked",
#                   FTcutoff=0.5,
                   stringsAsFactors =F)
scen_col <- ncol(scen)
for(kk in (scen_col+1):(scen_col+nrow(feat.lst)))
  scen[,kk] <- 17
names(scen)[(scen_col+1):ncol(scen)] <- feat.lst$name





