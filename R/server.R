##################################################################
##################################################################
## Shiny App for NPLCC prioritization
##
## Author: Richard Schuster (mail@richard-schuster.com)
## 22 January 2016
##
## v0.1 
##    - based on CDFCP.v0.17.3
##    - includes 3 eBird species for testing (OFSL, PSFL, TOWA)
## v0.3
##    - allow for different scales
## v0.4
##    - include SPF, Target table
## v0.5
##    - include tree PCA in community scores
##    - include future climate option
##    - ditch different scales and prepare for Gurobi
## v0.10
##    - Switch from Marxan to ILP
## v0.11
##    - Clean code
##    - store raster files in global location (pulayer)
## v0.13
##    - Make rhandsontables more restrictive
##    - resolve leaflet display issue (had to do with projection)
##    - change input values from fraction to %
##################################################################
##################################################################


# Define server logic 
shinyServer(function(input, output, session) {
#  system(sprintf("touch %s/restart.txt",globWD))
  #  setwd("/var/shiny-server/www/examples/calib.shiny.v2/")
  values = reactiveValues(
    hot_feat = feat.lst,
#    hot_tree = tree.lst,
    hot_multi = scen
  )
  #setHot = function(x) values[["hot"]] <<- x
  
  calc = reactive({
    # load initial values
    df1 = values[["hot_feat"]]
    #df2 = values[["hot_tree"]]  
    df3 = values[["hot_multi"]]
    
    
    list(feat = df1,
     #    tree = df2,
         multi = df3)
  })

  #######################
  ## Edit Targets
  #######################
  output$hot_feat = renderRHandsontable({
    if (!is.null(input$hot_feat)) {
      DF = hot_to_r(input$hot_feat)
      values[["hot_feat"]] = DF      
    } else if (!is.null(values[["hot_feat"]])) {
      DF = values[["hot_feat"]]
    }

    #prevent rhandson from adding rows when user drags values
    if(nrow(DF) > l.feat.lst){
      DF <- DF[1:l.feat.lst,]
      values[["hot_feat"]] = DF      
    }
    
    #setHot(DF)
    rhandsontable(DF, readOnly = T) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(c("Percent"), readOnly = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      #        hot_validate_numeric(col = 2, min = 0, max = 1.0) #%>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (col == 1 && (value > 100 || value < 0)) {
            td.style.background = 'red';
           }
         }")
  })


  #######################
  ## Multiple Scenarios
  #######################
  output$hot_multi = renderRHandsontable({
    if (!is.null(input$hot_multi)) {
      DF = hot_to_r(input$hot_multi)
      values[["hot_multi"]] = DF  
    } else if (!is.null(input$scen_file)){
      DF = read.csv(input$scen_file$datapath,stringsAsFactors =F)
      values[["hot_multi"]] = DF  
    } else if (!is.null(values[["hot_multi"]])) {
      DF = values[["hot_multi"]]
    }
    
    rhandsontable(DF, readOnly = F) %>%
#      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_table() %>%
      #hot_col(col = "time", type = "dropdown", source = c("curr","rcp45")) %>%
      hot_col(col = "cost", type = "dropdown", source = c("landc","area")) %>%
      #hot_col(col = "protected", type = "dropdown", source = c("locked","avail")) %>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (col > 0 && (value > 100 || value < 0)) {
            td.style.background = 'red';
           }
         }")
      #hot_col(c("include"), readOnly = FALSE) 
  })
  
  #######################
  ## Gurobi reactive
  #######################
  my.data <- reactive({ 
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.    
    if(input$mrun == 0)
      return(NULL)    
    
    return(isolate({

      if (input$MultiScen == FALSE) {
        feat.temp <- calc()$feat
        scnm <- "tt"
        scen[1,] <- c(scnm,input$cost,feat.temp$Percent)
      } else {
        scen <- calc()$multi
      }
      
      #debug
      write.csv(scen,"./output/scenarios.csv",row.names = F)
      
      progress <- Progress$new(session)
      progress$set(message = 'Setting up Analysis inputs', detail = "Please be patient...", value = 0.01)

      #setup output frames
      sel.fr <- data.frame(id=pu$id)
      res.fr <- data.frame(scen=character(),
                           cost=character(),                           
                           status=character(),runtime=numeric(),
                           cost_out=numeric(),
                           area=numeric(),stringsAsFactors=F)
      in_col <- ncol(res.fr)
      for(kk in (in_col+1):(in_col+ncol(puvsf)-1))
        res.fr[,kk] <- numeric()	
      names(res.fr)[(in_col+1):ncol(res.fr)] <- paste0(names(puvsf)[-1],"_Tar")
      
      in_col <- ncol(res.fr)
      for(kk in (in_col+1):(in_col+ncol(puvsf)-1))
        res.fr[,kk] <- numeric()	
      names(res.fr)[(in_col+1):ncol(res.fr)] <- names(puvsf)[-1]      
      
      #scenario loop
      for (ii in 1:nrow(scen)){

        pu_temp <- pu
        
        if(scen$cost[ii] == "area")
          pu_temp$cost <- 1
        

        puvsfeat.temp <- puvsf
        #prepare feat for cutoff

        #feat.temp <- sprintf("NPLCC_comm_feature_input_%s.csv",scale_temp)
        feat.temp <- data.frame(id=seq(1,ncol(puvsfeat.temp[,-1])),
                                Percent=as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)])),
                                name=names(puvsfeat.temp[,-1]),
                                stringsAsFactors =F)
        
        #debug
        write.csv(feat.temp,sprintf("./output/feat.temp_%s.csv",ii),row.names = F)
        
        progress$set(message = 'Calculation in progress', detail = sprintf("Scenario %s/%s",ii,nrow(scen)), 
                     value = round(ii/nrow(scen)*0.8,1))
        
        result <- fit.gurobi(pu=pu_temp, puvsfeat=puvsfeat.temp, feat=feat.temp)      

        if (input$MultiScen == FALSE) {
          scnm <- sprintf("T%s",as.character(mean(feat.temp$Percent)))
        } else {
          scnm <- scen$scenario[ii]
        }  
        
        sel.fr <- cbind(sel.fr,result$x)
        names(sel.fr)[ncol(sel.fr)] <- scnm

        res.fr[ii,] <- c(scnm, 
                         scen$cost[ii],
                         result$status, round(result$runtime,0), 
                         result$objval,
                         round(sum(result$x>0)/length(result$x)*100,2),
                         feat.temp$Percent,
                         round(colSums(puvsfeat.temp[result$x>0,-1])/
                         colSums(puvsfeat.temp[,-1])*100,2)
                         ) 
      }
      
      ################################################
      ##create rasters
      ################################################
      progress$set(message = 'Post processing', detail = "This will take a few mins...", value = 0.9)
      
      r <- in.raster 
      rv <- getValues(r)
      ind.r.v <- data.frame(id=idx.r.val[!is.na(idx.r.val)])
      
      res <- join(ind.r.v,sel.fr,by="id")
      
      rout <- list()
      for(ii in 2:ncol(res)){
        
        rv[!is.na(rv)] <- res[,ii]
        
        r[] <- rv
        rout[[ii-1]] <- r
      }
      rst <- stack(rout)
      names(rst) <- names(res)[-1]


      
      progress$set(value = 1)
      progress$close() 
      
      list(sel.fr=sel.fr,res.fr=res.fr,rst=rst,rstL=rst)
    }))

  })
  
  observe ({  my.data()
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    "Marxan results"
  })

  output$cadMap <- renderLeaflet({
    if(input$mrun == 0) {
      #print("Run Marxan")
      return(NULL)
    }

    
    #[[1]] for now, should allow for multiple eventually
    rst <- my.data()$rstL
    
    pal <- colorFactor(c('#d7191c','#2c7bb6'),domain=factor(values(rst[[1]])),
                        #pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                        na.color = "transparent")
    
    outl <- leaflet() %>% addTiles() %>%
      # Base groups
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Thunderforest.Landscape", group = "Terrain")# %>%
      

      # Overlay groups
      for(ii in 1:length(rst@layers))
        outl <- addRasterImage(outl,rst[[ii]], colors=pal, opacity = 0.9, 
                               maxBytes = 8 * 1024 * 1024, group = names(rst)[ii], project = FALSE)
    
        outl <- addLegend(outl, pal = pal, values = factor(values(rst[[1]])),labels=c("no","yes"),
                              title = "selected") %>%
        addLayersControl(
          baseGroups = c("StreetMap", "Aerial", "Terrain"),
          overlayGroups = names(rst),
          options = layersControlOptions(collapsed = FALSE)
          )

    outl    
            # Overlay groups
    #prog$set(value = 1)
    
    
    # end individual run attribute table
    ########################################
    #prog$close()     
  })  
  
  output$InMap <- renderLeaflet({
    map
  })  
  
  

  ###############################
  # Summary Table + Download Results raster
  ###############################
  output$summary <- renderTable({ # to display in the "Summary" tab
    if(input$mrun == 0) {
      return(data.frame(Output="You need to run the prioritization first"))
    }
    
    my.data()$res.fr
    
  })  

  output$downloadSHP <- downloadHandler(
  
    filename = function() {
      paste('NPLCC_rasters.zip', sep='')
    },
    content = function(file) {
      rst <- my.data()$rst
      
      owd <- setwd("./output")
      file.remove(list.files())
      for (ii in 1:length(rst@layers))
        writeRaster(rst[[ii]], sprintf("%s.tif",names(rst)[ii]), format="GTiff", overwrite=TRUE)      
      zip("NPLCC_rasters.zip",list.files())
      setwd(owd)
      
      #for later once zipped rasters are available
      file.copy(paste(getwd(),"/output/NPLCC_rasters.zip",sep=""), file)      
    }
  )

  output$download_ssoln <- downloadHandler(

    filename = function() {
      paste('NPLCC_prioritization_results-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$res.fr, file, row.names=F)
    }    
  )

  #Tabsets
  output$tabsets <- renderUI({
    tabs <- list(NULL)

    if(input$MultiScen == TRUE){
      tabs[[1]] <- tabPanel("Scenario List", rHandsontableOutput("hot_multi",width="100%",height="500px"))
      ii <- 1
    } else {
      tabs[[1]] <- tabPanel("Edit Target", rHandsontableOutput("hot_feat"))
      #tabs[[2]] <- tabPanel("Edit Trees", rHandsontableOutput("hot_tree"))
      ii <- 1
    }
    #tabs[[ii+1]] <- tabPanel("Input Layers",leafletOutput("InMap",height=900))
    #tabs[[ii+2]] <- tabPanel("Tree Layers",leafletOutput("TreeMap",height=1000))
    #tabs[[ii+3]] <- tabPanel("Tree Community",leafletOutput("TreeMapTool",height=1000))
    tabs[[ii+1]] <- tabPanel("Results + Download",
                     helpText(HTML("<h4>Result Summary Table</h4>")),
                     tableOutput("summary"),
                     helpText(HTML("<h4>Download output raster:</h4>")),
                     downloadButton("downloadSHP", label = "Raster"),
                     helpText(HTML("<br>")), 
                     helpText(HTML("<h4>Results download (summary of outputs):</h4>")),
                     downloadButton("download_ssoln",label = "Results download"),
                     helpText(HTML("<br>"))
                    )
    tabs[[ii+2]] <- tabPanel("Result Map",leafletOutput("cadMap",height=900))
    
    tabs$id <- "tab0"
    do.call(tabsetPanel, tabs)
  })  
  

})


