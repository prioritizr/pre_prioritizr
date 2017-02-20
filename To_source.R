#' Ingest a shapefile to work with the prioritization apps
#'
#' @param shp A number.
#' @param outloc Where do you want to save the output files of ingest.
#' @return Nothing.
#' @examples
#' ingest(tt,getwd())
#'
ingest <- function(shp="",outloc=getwd()){

  dat <- shp@data
  id <- seq(1,nrow(dat),1)
  shp@data$Idx <- id

  #pu file
  pu <- data.frame(id=id,cost=dat$cost,status=dat$status)

  #start puvsfeat file
  featin <- data.frame(pu=id,dat[,3:ncol(dat)])
  puvsfeat <- gather(featin,species,value,-pu)

  #feat file
  feat <- data.frame(id=unique(as.numeric(as.factor(puvsfeat$species))),
                         prop=0.0,
                         spf=1,
                         name=unique(puvsfeat$species))

  #continue puvsfeat file creation and cleaning
  puvsfeat$species <- as.numeric(as.factor(puvsfeat$species))
  puvsfeat <- puvsfeat[order(puvsfeat$pu,puvsfeat$species),]

  puvsfeat <- puvsfeat[puvsfeat$value > 0,]
  puvsfeat <- puvsfeat[,c("species","pu","value")]

  # generate the boundary length file
  blf <- calcBoundaryData(shp)
  blf <- blf[order(blf$id1),]

  #generate raster
  shp_ar <- gArea(shp,byid=T)

  r <- raster(extent(shp))
  res(r)= sqrt(mean(shp_ar))
  r <- rasterize(shp,field="Idx",r)

  ####################################################
  ####################################################
  owd <- setwd(outloc)

  write.csv(pu,"pu.csv",row.names=F)
  write.csv(puvsfeat,"puvsfeat.csv",row.names=F)
  write.csv(featin,"puvsf.csv",row.names=F)
  write.csv(feat,"feat.csv",row.names=F)
  write.csv(blf,"bound.csv",row.names=F)
  writeRaster(r, filename="idx.tif", format="GTiff", overwrite=TRUE)

  setwd(owd)

}

#' Ingest a csv file to work with the prioritization apps
#'
#' @param shp A number.
#' @param outloc Where do you want to save the output files of ingest.
#' @return Nothing.
#' @examples
#' ingest(tt,getwd())
#'
ingest.csv <- function(csv="",outloc=getwd(),crs=CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")){

  dat <- read.csv(csv)
  id <- seq(1,nrow(dat),1)
  #shp@data$Idx <- id

  #pu file
  pu <- data.frame(id=id,cost=dat$cost,status=dat$status)

  #start puvsfeat file
  datf <- dat[ ,-which(names(dat) %in% c("pu","cost","status","lat","long"))]
  featin <- data.frame(pu=id,datf)
  puvsfeat <- gather(featin,species,value,-pu)

  #feat file
  feat <- data.frame(id=unique(as.numeric(as.factor(puvsfeat$species))),
                         prop=0.0,
                         spf=1,
                         name=unique(puvsfeat$species))

  #continue puvsfeat file creation and cleaning
  puvsfeat$species <- as.numeric(as.factor(puvsfeat$species))
  puvsfeat <- puvsfeat[order(puvsfeat$pu,puvsfeat$species),]

  puvsfeat <- puvsfeat[puvsfeat$value > 0,]
  puvsfeat <- puvsfeat[,c("species","pu","value")]

  # generate the boundary length file
  #blf <- calcBoundaryData(shp)
  #blf <- blf[order(blf$id1),]

  #generate raster
  tt <- rasterFromXYZ(data.frame(x=dat$long,y=dat$lat,id=seq(1,length(dat[,1]))),crs=crs)
#  nrw <- length(unique(dat$lat))
#  ncl <- length(unique(dat$long))
#  xmn <- min(dat$long)
#  xmx <- max(dat$long)
#  ymn <- min(dat$lat)
#  ymx <- max(dat$lat)

  #create raster from csv
#  tt <- raster(nrows=nrw, ncols=ncl, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)

  #set values for raster
#  tt[] <- id

  ####################################################
  ####################################################
  owd <- setwd(outloc)

  write.csv(pu,"pu.csv",row.names=F)
  write.csv(puvsfeat,"puvsfeat.csv",row.names=F)
  write.csv(featin,"puvsf.csv",row.names=F)
  write.csv(feat,"feat.csv",row.names=F)
  #write.csv(blf,"bound.csv",row.names=F)
  writeRaster(tt, filename="idx.tif", format="GTiff", overwrite=TRUE)

  setwd(owd)

}

#' Integer Linear Programming Shiny App
#'
#' @param infolder Location of the input files folder created by ingest or supplied.
#' @param appfolder Location of the folder where the Shiny app should reside.
#' @param appfiles Just for now, will eventually be mute.
#' @return Nothing.
#' @examples
#' ILPapp(infolder=outloc,appfolder=app,appfiles=appf)
#'

#source functions in package
#package internal functions

#create output folder if it does not exist in app folder

ILPapp <- function(infolder="",appfolder=getwd(),appfiles=""){

  #create app folder
  dir.create(file.path(appfolder), showWarnings = FALSE)

  #create data folder in appfolder
  dir.create(file.path(appfolder, "data"), showWarnings = FALSE)
  #create output folder in appfolder
  dir.create(file.path(appfolder, "output"), showWarnings = FALSE)

  #copy input files from infolder to appfolder
  owd <- setwd(infolder)
  file.copy(from=list.files(), to=paste(appfolder,"data",sep="/"))
  setwd(owd)

  #copy app files to appfolder
  owd <- setwd(appfiles)
  file.copy(from=list.files(), to=appfolder,recursive=T)

  setwd(appfolder)

  runApp()
}
