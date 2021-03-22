
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)




options(shiny.maxRequestSize=600*1024^2)
shinyServer(function(input, output, session) {
 
  parsed_data <- NULL
  summary_data <- NULL
  processed_data <- NULL
  rejected <- NULL
  #plot_comments <- NULL
  
   session$onSessionEnded(function() {
     unlink("ImageAppoutput",recursive = TRUE)
     unlink("results.zip")
    stopApp()
  })
  
   
   plot_comments <-reactiveValues()
   
   data_to_plot <- reactive({
     req(input$reject_compound)
     plot_data(processed_data,input$reject_compound,plot_comments)
   })
   
   
   
  observeEvent(input$validate,{
    
    account <- CoreAPIV2::coreAPI(input$target)
    
    print(account)
    print(input$target)
    
    account$user <- input$user
    account$pwd<- input$password
    
    output$logmessage <- renderText(
      tryCatch(
        {login <- CoreAPIV2::authBasic(account)
        lo<-CoreAPIV2::logOut(login$coreApi)
        
        "Credentials Validated"
        }
        ,
        error =function(cond){
          return("Credentials not accepted")  
        } 
        
      )
    )
    
     
    
  })
  
  output$fileinput <- renderUI({  
  
    if(input$file_type == "dual"){
    
    tagList(
  fileInput("gfp_file", 'Choose GFP+ Data File',        #small File
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv','application/zip')),
  
  fileInput("total_file", 'Choose Total Area Data File',        #large file
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv','application/zip'))
    )
    } else
      
      tagList(
        fileInput("image_file", 'Choose Image Data File',        #dual file
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv','application/zip'))
        
        
      )
  
  }) 
  
  output$status <- renderText({ 
    
    if (is.null(input$gfp_file)) "Select inputs" else "Hit Process"
    })
  
 
  # Load data and create parsed data which is content of All_data, create UI for selecting contols
  
  observeEvent(input$load,{
    
    if ( (is.null( input$gfp_file) & is.null(input$image_file)) )
    {
    
      return()
       
    } else {
    
      
    
    #process data
    
      output$status <- renderText("Processing Data")
      
      progress <- Progress$new(session)
      on.exit(progress$close())  
      

       parsed_data <<-all_data_script(input$gfp_file$datapath,input$total_file$datapath,input$image_file$datapath,
                       input$plate_file$datapath,getwd(),input,progress)
                      
              
                        

      
      
                      
     #Create Contols UI
                      
                      parsed_data <- parsed_data$parsed_data
                      compounds <- unique(parsed_data$var_compound[is.na(parsed_data$control) & !is.na(parsed_data$concentration)])
                      
                      controls <-c("none", unique(parsed_data$concentration[!is.na(parsed_data$control)]) )
                      
                   
                      
                      v<-list()
                      
                      for(i in 1:length(compounds)){ 
                        v[[i]] <-
                          selectInput(inputId = paste0("compound",i),label = compounds[i],choices = controls,selected = "none") 
                        
                      }
                      
                      v[[i+1]] <- actionButton("process","Process Data")
                      
                      output$controls <- renderUI(v)
                      
                      
                      
                      
    
      output$status <- renderText("Loading Completed")
    }
      
      
    })
  
  observeEvent(input$useplatefile,{
    
     if(input$useplatefile){
       enable("plate_file")
       updateTextInput(session,"plate_barcode",value="")
       disable("plate_barcode")
       
     }else{ 
        enable("plate_barcode")
        disable("plate_file")
     }
  })
 
  
  # Use parsed_data (All_data) to create summary data and plots, and zip file
  
  observeEvent(input$process,{
    
    
    if (  is.null(input$gfp_file)  & is.null(input$image_file) )
    {
      
      return()
      
    } else {
      

      #process data
      
      output$status <- renderText("Processing Data")

      #  suppressWarnings({ 
      progress <- Progress$new(session)
      on.exit(progress$close())  
      
      processed_data <<- summary_data(parsed_data,getwd(),input,progress)

      ### run rejections here
      
      
      output$reject<- renderUI({

        summary_data <- processed_data$summary
        
        well_data <- processed_data$well 
        

        
        compound_list <- (
          well_data %>% filter( is.na(well_data $control) &  !is.na(var_compound) ) %>% 
            select(var_compound) %>% distinct()
        )[,1] 
        
        output$reject_plot <- renderPlot( percent_plot_reject(data_to_plot()))
        
        fluidPage(
        fluidRow(
        column(3,selectInput("reject_compound",label = "Compound",choices = compound_list)),
        column(7,plotOutput("reject_plot", click = "plot1_click"))
        ),       
        fluidRow(
                 column(3,offset = 3,textInput("comment","Comment")), 
                 column(3,textOutput("click_info"))
                 ),
        fluidRow(
                  column(3,offset = 3,actionButton("add_comment","Add Comment"))
          ),
         hr(),
        fluidRow(
            column(2,offset = 3,actionButton("write","Create Output")),
            column(3,disabled(downloadButton("downloadData", "Download")))
        )
        )
        })

       
    updateTabsetPanel(session,"inTabset", selected = "Review Data"
                      
                      )  
      
    }
    
    
  })
  
  observeEvent(input$write,{
    
    
  #recalculate summary data with rejected points removed
    progress <- Progress$new(session)
    on.exit(progress$close())  
    
    final_data <- summary_data(list(
                                    parsed_data=processed_data$well,
                                    plate=parsed_data$plate),
                                    getwd(),input,progress)
    #write summary and all_ data  
    
    filePath <- paste0(getwd(),"/ImageAppoutput")  
    
    if(!dir.exists(filePath)) dir.create(filePath)   
  
    
    write.csv(final_data$summary,file=paste0(filePath,"/final_summary_data.csv"),row.names = FALSE )
    
    
    write.csv(final_data$well, file=paste0(filePath,"/final_all_data_data.csv"),row.names = FALSE )
  

    

    make_plots(final_data,
                     getwd(),
                     progress,plot_comments)
    
    
    zip::zip("results.zip","ImageAppoutput",recurse = TRUE)
    
    output$status <- renderText("Analysis Completed")
    
    enable("downloadData")
    
    
  })
  
  
  
#### Download output  
  
 
    output$downloadData <- downloadHandler(
     
       filename = function() {
      
        paste0("Results-",format(Sys.time(),"%Y-%m-%d--%H:%M"),".zip")
      },
      content = function(file) {
        
        file.copy(from = "results.zip", to = file)
        
      }
      ,contentType = "application/zip"
    )
    
 
    
  
      output$click_info <- renderPrint({
      # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      # were a base graphics plot, we'd need those.
     
     near <- nearPoints(data_to_plot()$compound_percent_data, input$plot1_click, addDist = TRUE)
    
   
      
     near_wells <- near$Well 
     
      
      current_rejected <- processed_data$well$rejected
      
    well_to_change <-  processed_data$well$rejected[which(processed_data$well$Well ==  near_wells)] 
    
    
   
      
      processed_data$well$rejected[which(processed_data$well$Well ==  near_wells)] <<- 
           ifelse(  processed_data$well$rejected[which( processed_data$well$Well   ==  near_wells)],0,1)
     
       well_to_changeed <-  processed_data$well$rejected[which(processed_data$well$Well ==  near_wells)] 
      
      output$reject_plot <- renderPlot( percent_plot_reject( 
                    plot_data(processed_data,input$reject_compound,plot_comments)))
      
      
     return( ifelse(length(near_wells) > 0,near_wells,"") )
      
      
    })
    
  
      
    observeEvent(input$add_comment,{
      
    plot_comments[[input$reject_compound]] <<- input$comment
    
    updateTextInput(session, "comment",value = "")
      
 #   output$reject_plot <- renderPlot( percent_plot_reject(data_to_plot()))
    
    })
    
    
    
    
    
    observeEvent(input$total_file,{
      
 
      
      if ( !is.null( input$total_file) & !is.null( input$gfp_file)) { enable("load")}
      
              })
          
    observeEvent(input$gfp_file,{
      
     
      if ( !is.null( input$total_file) & !is.null( input$gfp_file)) { enable("load")}
      
    })
    
   
    observeEvent(input$image_file,{

      if ( ( !is.null(input$image_file) & (input$file_type == "single") )) { enable("load")}
      
    })
    
     
  })
  
  



