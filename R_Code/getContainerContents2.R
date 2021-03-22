# 
# 
# rm(list=ls())
# source("global.R")
# 
# api <- CoreAPIV2::coreAPI("Credentialsfreq_prod.txt")
# 
# api$user <- ""
# api$pwd<- ""
# 
# creds<- CoreAPIV2::authBasic(api)$coreApi
# 
# barcode <- "WCP1"
# 
# useVerbose <- T
# 
# 
# 


getContainerContents2<-function (creds, barcode, useVerbose = FALSE,progress)
{
  
  #Get plate with cell ids
  
  
 
   resource <- "CONTAINER"
  
  #alist <- paste0(associations, collapse=",")
  
  query <- 
    paste0("('",barcode,"')/REV_IMPL_CONTAINER_CELL")
  
  
 
 # print(query)
  
    header <- c(Accept = "application/json;odata.metadata=full")

cells <-
    CoreAPIV2::apiGET(
      creds,
      resource = resource,
      query = query,
      headers = header,
      useVerbose = useVerbose
    )
  
  cellIDs <- unlist(lapply(cells$content, function(x) x$Id))
  
  nCells <- length(cellIDs)
  
 cellContents <- list()
 sampleLots  <- list()  
 sampleType <- list()
control <- list()

 resource <- "CELL"
 header <- c(Accept = "application/json;odata.metadata=full")
 
 
#Do to error in API (lots with fractional concetration can not be retrieved by the API,
# adding a json call to get the lots.
 
#Need json authentications
 
 
 
creds_json  <- V1_authBasic(creds)$coreApi
 
 
 
 
 
  for (i in 1:nCells){
    
  #get each cell contents
     
  print(i)
 
   query <- 
     paste0("(",cellIDs[i],")/CONTENT?$expand=IMPL_SAMPLE_LOT")
   
   
   
  # print(query)
   
   
   
   cell <-
     CoreAPIV2::apiGET(
       creds,
       resource = resource,
       query = query,
       headers = header,
       useVerbose = useVerbose
     )
   
    cellContents[[i]] <- cell$content[[1]]
     
   #get sample lot
   #ugly hack because cells don't report sample type
    
  
    prefix <- substr(cellContents[[i]]$IMPL_SAMPLE_LOT$Barcode,1,3)
    
      type <- switch(prefix,
           TRT = "TREATMENT_LOT",
           BLK = "BLANK_LOT"
           )
    
    
 #removed for bug workaround
     #lot <- CoreAPIV2::getEntityByBarcode(creds,type,cell$content[[1]]$IMPL_SAMPLE_LOT$Barcode,fullMetadata = TRUE,useVerbose = T)
    
      lot<- jsongetEntityByBarcode(creds_json,type,cell$content[[1]]$IMPL_SAMPLE_LOT$Barcode,useVerbose = T)
      
    sampleLots[[i]] <- lot$entity
    sampleLots[[i]]$Barcode <-  sampleLots[[i]]$barcode  #The json API has a different capitalization 
    
    sampleType [[i]] <- type
    
    progress$inc(amount =  0.004)
    Sys.sleep(.1)
    
  }  
 
 
 
 
sampleLot <- unlist( lapply(sampleLots, function(x) {x$barcode} ) )
 
#####################

 print(sampleLot )

###########################

var_compound <- unlist( lapply(sampleLots, function(x) {getVarcompound(x)} ) )
compound <-  unlist( lapply(sampleLots, function(x) {buildCompound(x)})) #will need to pass concentration and units if we change them 
content <-  unlist( lapply(sampleLots, function(x) {buildContents(x)}))


#scale concentrations if needed 

concentration <-unlist( lapply(sampleLots, function(x) {getConcentration(x)} ) )

if (max(as.numeric(concentration)) > 1000000){
  concentration <- as.character(as.numeric(concentration)/1000)
  units <- rep("uM",nCells)
 } else units <- rep("nM",nCells)



#Asume all controls are in row H 

control <- rep(NA,nCells)

lastrownames <-  unique(sampleLot[85:96])

lastrownames <- lastrownames[substr(lastrownames,1,3) != "BLK" ]

#add CTRL to content string
for (i in 85:96)
{
  
  if (sampleLot[i] %in% lastrownames ) {
    
    control[i] <- as.character(which(lastrownames == sampleLot[i]))
    
  }
}
 
#copy contnet to compound, var_compound, and concentration

compound[which(!is.na(control))] <- content[which(!is.na(control))]

var_compound[which(!is.na(control))] <- content[which(!is.na(control))]

content[which(!is.na(control))] <-content[which(!is.na(control))]

concentration[which(!is.na(control))] <- paste0("CTRL",control[which(!is.na(control))],"_",content[which(!is.na(control))])


#make var_compound  and concentration the same as compound for controls

#var_compound[which(!is.na(control))] 

#Deal with all blanks

units[ which(sampleType == "BLANK_LOT")] <- NA
content[ which(sampleType == "BLANK_LOT")] <- NA
var_compound[ which(sampleType == "BLANK_LOT")] <- NA
compound[ which(sampleType == "BLANK_LOT")] <- NA
concentration[ which(sampleType == "BLANK_LOT")] <- NA


p <- makePlate()

plate_map <- data.frame(  
                          content = content,
                          compound = compound,
                          var_compound = var_compound,
                          concentration =concentration,
                          units = units,
                         control  = control,
                         stringsAsFactors = FALSE
)
plate_map <- cbind(p,plate_map)
 

##############3
print(plate_map)
##########################3

return(plate_map)  
  
}