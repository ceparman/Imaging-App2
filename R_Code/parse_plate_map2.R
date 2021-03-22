parse_plate_map2 <-function(file,debug=FALSE)
  
{
  
  
  
  t<-read.csv(file,stringsAsFactors = FALSE,header=FALSE,fill = TRUE)
  plate_name <- t[1,2]
  
  
  plate_map <- makePlate()
  
  plate_map$content <- ""
  plate_map$compound <-""
  plate_map$var_compound <-""
  plate_map$concentration <-"0"
  plate_map$units <- ""
  plate_map$control <-""
  
  
  for(r in seq(4,18,2))
  {
    
    for (c in 2:13)
    {
      # Well <-  gsub(" ", "", paste0("Well =",t[r,1],as.character(c)), fixed = TRUE)
      #  print(Well)
      
      Well <- gsub(" ","",paste0(t[r,1],as.character(c-1)))
      index <- which(plate_map$Well == Well) 
      
      contents <- t[r,c]
      
      if( is.na(contents) | (contents == "0") ) 
      {
        contents <- NA
        concentration <- NA
        units <- NA
        compound <- NA
        var_compound <- NA
        control <- NA
        
      }else {
        ##do we have a control  
        
        if(grepl("^CTRL",contents))
        {
          #process control
          concentration <- str_sub(contents,7,str_length(contents)) #this is what to show on plot        compound <-str_sub(contents,7,str_length(contents))
          units <- NA
          compound <- str_sub(contents,7,str_length(contents))
          var_compound <- compound #need something to index by
          control <- str_sub(contents,5,5)
          
        }else
        {
          #process cell
          
          
          conc <- str_split(contents,":")[[1]][2]  
          
          concentration <- as.numeric( str_sub(conc,
                                               1,str_locate_all(conc," ")[[1]][1]-1 ))
          
          units <- str_sub(conc,
                           str_locate_all(conc," ")[[1]][1]+1,
                           str_locate_all(conc," ")[[1]][2]-1)
          
          compound <- str_split(contents,":")[[1]][1]  
          var_compound <- str_sub(conc,
                                  str_locate_all(conc," ")[[1]][2]+1,
                                  str_length(conc))
          
          control <- NA
          
          
        } #end process cell
        
        
        
      } #end not empty
      
      
      
      
      
      
      
      
      plate_map$content[index] <- contents
      plate_map$compound[index] <- compound
      plate_map$var_compound[index] <- var_compound
      plate_map$concentration[index] <- concentration
      plate_map$units[index] <- units
      plate_map$control[index] <- control
      
      
      if(debug) print(plate_map[index,])
      
    } #end column
    
    
    
  } #end row
  plate_map
  
}