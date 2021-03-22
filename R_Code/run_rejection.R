run_rejection <- function(processed_data,input,output)
{
  
  summary_data <- processed_data$summary
  
  well_data <- processed_data$well 
  
  rejected <- data.frame( Well = well_data$Well, rejected = rep(0,nrow(well_data)))
  
  
  compound_list <- (
    well_data %>% filter( is.na(well_data $control) &  !is.na(var_compound) ) %>% 
      select(var_compound) %>% distinct()
  )[,1] 
  
   output$reject_select<- renderUI(selectInput("reject_compound",label = "Compound",choices = compound_list))
  
  
   output$reject_plot <- renderPlot( percent_plot_reject(data_to_plot(),rejected))
  
  
  
  
  
  
  
  
  
  
NULL
  
}
  
  
  