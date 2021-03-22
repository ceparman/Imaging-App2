
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  # Application title
  titlePanel(title=div(img(src="logo-frequency-therapeutics.png"))
             ,windowTitle = "Imaging App 2"),

  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout( 
    sidebarPanel(
      tags$h4("Cell Imaging Analysis"),
      tags$hr(),
      
      fluidRow(
       column(4,  textInput("user", "enter user name","")),
       column(4,  passwordInput("password","enter password",""))       
     ),  
     fluidRow(
       column(4,  actionButton("validate", "validate credentials")),
       column(4,  textOutput(outputId = "logmessage")) 
     ),
     fluidRow(column(8,
                     radioButtons("target","Select Tenant",
                                  choiceNames =  c("Production","Testing"),
                                  choiceValues = c("Credentialsfreq_prod.txt","Credentialsfreq_test.txt"),
                                  inline = T)
                     ),
              ),
     
  
      tags$hr(),
     
     textOutput("status"),
     tags$hr(),
     fluidRow(
     column(4,textInput("plate_barcode", "enter plate barcode",value = "WCP31")),
     column(4,checkboxInput("useplatefile","Use Plate File",value = FALSE))
     ),
     
     
     disabled(fileInput("plate_file", 'Choose Plate Data File',        #plate File
               accept=c('text/csv', 
                        'text/comma-separated-values,text/plain', 
                        '.csv','application/zip'))),
    tags$hr(),
     
    radioButtons( "file_type", "Single or Dual Files", choices = NULL, selected = NULL,
                 inline = TRUE, width = NULL, choiceNames = c("Single File","Two Files"),
                 choiceValues = c("single","dual")),
    
    uiOutput("fileinput"),
    
    
    tags$hr(),
    
   # textInput("path", "Directory for Saved Data"),
    
   
    
    tags$hr(),
   
    disabled(actionButton("load","Load Data and Sample Info"))
    
    
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(id = "inTabset",
      tabPanel("Controls",
         uiOutput("controls") #, 
        #  actionButton("process","Process Data")
      ),
      tabPanel ("Review Data",
        uiOutput("reject") #,
       # textOutput("click_info"),
       # actionButton("write","Write Results"),
       # downloadButton("downloadData", "Download")
       )
    )
    )
      
      
  
  )
))
