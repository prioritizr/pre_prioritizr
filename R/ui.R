
# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Mammal Prioritizataion v0.1"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
#    strong(textOutput("clickcoordsnmds")),
    #submitButton("Run Marxan")
    actionButton("mrun",HTML("<h4>Run Optimization</h4>")), 
    helpText("================================="), 
    checkboxInput("MultiScen", "Run multiple scenarios"),
    conditionalPanel(
      condition = "input.MultiScen == true",  
      helpText(HTML("You can either set the scenario parameters directly in the 'Scenario List' table on the right, or upload your scenario file below.<br>
                    Make sure that your file follows the same structure as the table on the right.<br>
                    For details about the columns please refer to the tool manual.")),
      fileInput('scen_file', 'Choose scenario file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'))
    ),
    conditionalPanel(
      condition = "input.MultiScen == false",
      #actionButton("tree.update",HTML("Update tree community input")), 
      helpText("================================="),
      helpText(HTML("<h4><strong>Global parameters:</strong></h4>")),
      selectInput("cost", "What cost metric should be used:",
                  c("Area" = "area",
                    "Landcover metric" = "landc"
                    ))

  )),
  
  # Outputs
  mainPanel(
    #Setup in Server.R
    uiOutput("tabsets")
  )
))
