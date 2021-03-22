plot_data <- function(processed_data,test_compound,plot_comments)
{
  summary_data <- processed_data$summary
  
  well_data <- processed_data$well 
  
  summary_data2 <- summary_data %>% filter(var_compound == test_compound    | !is.na(control))
  
  percent_data <- well_data %>% filter(var_compound == test_compound    | !is.na(control))  
  
  #if(!is.null(plot_comments)) {print(plot_comments)}
  
  if(is.null(plot_comments)){
    plot_comment <- ""
  } else {
    
   if(test_compound %in% names(plot_comments))  {
     plot_comment <-   plot_comments[[test_compound]]
  } else plot_comment <- ""
   }
  
  compound_percent_data <- percent_data %>% mutate( concentration = ifelse( is.na(control),concentration,0) ) %>% 
    group_by(content) %>% mutate( x_label = ifelse( is.na(units), "Controls", paste(concentration,units) )) %>%
    mutate( cat = ifelse( is.na(units),  "green","red")) %>%
    mutate(plotorder = ifelse(is.na(control), (as.numeric(concentration,rm.na=TRUE)*10000 +10), as.numeric(control,rm.na=TRUE)))
  
 
  
 compound_percent_data <- compound_percent_data %>% filter ( !is.na(plotorder)) 
  
   compound_percent_data$x <-  reorder(compound_percent_data$x_label, compound_percent_data$plotorder)
  compound_percent_data$y <- as.numeric(compound_percent_data$percent)
  
  range <- compound_percent_data %>% summarise( maxp = max(percent,na.rm = TRUE) , minp = min(percent,na.rm = TRUE),
                                                concentration = max(concentration))
  
 # var_data <- summary_data2 %>% filter( is.na(control)) %>%
#    mutate( x_label =  paste(concentration,units)) %>%
#    mutate( text_label  = stars(p_value)) %>%
#    merge(range)

  list(
       compound_percent_data= compound_percent_data,
       percent_data = percent_data,
       range = range,
       summary_data2  =summary_data2,
       plot_comment = plot_comment
       )
    
}
