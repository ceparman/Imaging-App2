do_ttest<-function(all_data,input,compounds,controls)
{

#remove blanks
  all_data <- all_data[!is.na(all_data$concentration),]
  
#remove rejected data
  
  all_data <- all_data %>% filter(rejected == 0)
  
  ttest_results <- data.frame()  #need to make this a zero row frame to take into account the
                                 #possibility of no controls/tttest
#loop over compounds    
for(i in 1:length(compounds)){
  
selected_control <-  eval(parse(text=paste0("input$compound",i)))

if(selected_control != "none" ){  
control_data <- all_data[all_data$concentration == selected_control,]

compound_data <- all_data[all_data$var_compound == compounds[i],]

concentrations <- unique(compound_data$concentration)

for(j in 1:length(concentrations))
{
  #select data
  
  #do ttest for percents
  
  test_data <- compound_data[compound_data$concentration == concentrations[j],]
  
  #do ttest for percents
   tt <- t.test(x = test_data$percent,y = control_data$percent,var.equal = TRUE)
   #print(tt)
   
   p <- ifelse(tt$estimate[1] > tt$estimate[2],tt$p.value,1.0)
  
 # print(p)
   
#   print(paste(test_data$var_compound[1], concentrations[j],p))
   
   #now do tttest on counts
   tt_counts <- t.test(x = test_data$gfp_count,y = control_data$gfp_count,var.equal = TRUE)

   
   p_counts <- ifelse(tt_counts$estimate[1] > tt_counts$estimate[2],tt_counts$p.value,1.0)
   
  
    #now do ttest on area
   tt_area <- t.test(x = test_data$gfp_area,y = control_data$gfp_area,var.equal = TRUE)

   
   p_area <- ifelse(tt_area$estimate[1] > tt_area$estimate[2],tt_area$p.value,1.0)
   
   
   
   #build results
   
   ttest_results <-rbind(ttest_results,data.frame(var_compound =test_data$var_compound[1],
                                  concentration =concentrations[j],
                                  p_value = round(p,4),
                                  p_value_counts = round(p_counts,4),
                                  p_value_area = round(p_area,4),
                                  control_compound =selected_control ))
   
}


}  
  
}  
  ttest_results
  
}