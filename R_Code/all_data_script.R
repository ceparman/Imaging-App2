
all_data_script <- function(gfp_file,total_file,dual_file,plate_map_file,filePath,input,progress)
  
{
  


### create directory if it does not exist
  
filePath <- paste0(filePath,"/ImageAppoutput")  
  
  if(!dir.exists(filePath)) dir.create(filePath)   
  

  
plate <- makePlate()

if(input$useplatefile) {
  


plate_map <- parse_plate_map2(plate_map_file ) 

}else {



#print(getwd())

api <- CoreAPIV2::coreAPI(input$target)

api$user <- input$user
api$pwd<- input$password


creds<- CoreAPIV2::authBasic(api)$coreApi 

barcode <- input$plate_barcode
#barcode <- "WCP31"

progress$inc(amount = 0.01,message = "Getting Plate Contents")

plate_map <-getContainerContents2(creds, barcode, useVerbose = FALSE,progress)



lo<- CoreAPIV2::logOut(creds)
}
progress$inc(amount = 0.05)

if(input$file_type == "dual"){
  data1 <- processReaderFile(gfp_file,plate,progress) 
  data2 <- processReaderFile(total_file,plate,progress) 
}
  else{
    data1 <- processReaderFileSingle(dual_file,plate,23,progress) 
    data2 <- processReaderFileSingle(dual_file,plate,17,progress) 
    
    print(paste("Dual file loaded ",nrow(data2 ),"rows"))
    
  }
#Combine data


all_data <- data.frame(Well=data1$Well,gfp_area=data1$`Total Area`,gfp_count=data1$`Total Count`,
                       allcells_area=data2$`Total Area`,allcells_count=data2$`Total Count`,
                       percent = 100*data1$`Total Area`/ data2$`Total Area`)
#Add columns reject

all_data$rejected <- rep(0,nrow(all_data))



### Write output

m<- merge(plate_map,all_data,by="Well")

m <- m[order(m$index),]

#write.csv(m,file=paste0(filePath,"/All_data.csv"),row.names = FALSE)  #We will need to move this write statement to after point rejections


list(parsed_data = m, plate = plate_map)

}




