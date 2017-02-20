library(rgeos)
library(rgdal)
library(tidyr)
library(marxan)
library(shiny)

source("To_source.R")
outloc <- "outloc"
app <- "app"

appfls <- "/R"

shp <- readOGR("./shp","Mamm_ingest")
ingest(shp,outloc)

#csv <- "D:/Work/Charlotte/errors/PrioritizR_csv/shp/2030_26_widebyspecies.csv"
#ingest.csv(csv,outloc)

ILPapp(infolder=outloc,appfolder=app,appfiles=appfls)
