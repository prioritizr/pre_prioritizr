library(rgeos)
library(rgdal)
library(tidyr)
library(marxan)
library(shiny)

source("D:/Work/Charlotte/errors/PrioritizR_csv/To_source.R")
outloc <- "D:/Work/Charlotte/errors/PrioritizR_csv/outloc"
app <- "D:/Work/Charlotte/errors/PrioritizR_csv/app2"

appfls <- "D:/Work/Charlotte/errors/PrioritizR_csv/R"

shp <- readOGR("D:/Work/R/PrioritizR/shp","Mamm_ingest")
ingest(shp,outloc)

#csv <- "D:/Work/Charlotte/errors/PrioritizR_csv/shp/2030_26_widebyspecies.csv"
#ingest.csv(csv,outloc)

ILPapp(infolder=outloc,appfolder=app,appfiles=appfls)
