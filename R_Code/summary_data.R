summary_data <- function(parsed_data,filePath,input,progress){
  
  filePath <- paste0(filePath,"/ImageAppoutput")  
  
  if(!dir.exists(filePath)) dir.create(filePath)   
  
  progress$inc(amount = 0.1,message = "Calculating replicate stats")
  
   m         <- parsed_data$parsed_data
  plate_map <- parsed_data$plate
  
  #calculate mean and STD of replicates
  averaged <- m %>% filter(!is.na(var_compound)) %>% filter(rejected == 0 ) %>%
    group_by(var_compound,concentration) %>%
    summarise(mean_gfp_area = mean(gfp_area,na.rm = TRUE), std_gfp_area = sd(gfp_area,na.rm = TRUE),
              mean_all_area =mean(allcells_area,na.rm = TRUE), std_all_area =sd(allcells_area,na.rm = TRUE),
              mean_percent = mean(percent,na.rm = TRUE)  , std_percent = sd(percent,na.rm = TRUE)
    ) 
  
  
  content <-  plate_map %>% filter(!is.na(var_compound)) %>% select(content, var_compound, control, concentration, units) %>% unique
  
  
  s<- left_join(averaged,content,by=c("var_compound","concentration"))
  
  s<- s[order(s$var_compound,s$concentration),]
  
  
  #do ttest on parsed_data
  
  compounds <- unique(m$var_compound[is.na(m$control) & !is.na(m$concentration)])
  
  controls <-c("none", unique(m$concentration[!is.na(m$control)]) )
  
  tt<-  do_ttest(m,input,compounds,controls)
  print(str(tt))
  
 # print(tt)
  
  progress$inc(amount = 0.1,message = "Writing Data")
  
  s <- merge(s,tt,all.x=T)
  
 # write.csv(s,file=paste0(filePath,"/summary_data.csv"),row.names = FALSE)
  
  
  processed_data <- list(summary=s,well=m)
  
  processed_data
 
  
  
  
  
  
}
