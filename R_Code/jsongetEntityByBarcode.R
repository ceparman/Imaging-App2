# entityType = "BLANK_LOT"
# barcode = "BLK1-1"
# coreApi <- creds
# 
# api <- CoreAPIV2::coreAPI("Credentialsfreq_prod.txt")
# 
# api$user <- ""
# api$pwd<- ""
# 
# 
# 
# coreApi  <- CoreAPI::authBasic(api)$coreApi
# 
# 
# 
# 
# 
# 
#  useVerbose=T


jsongetEntityByBarcode<-function (coreApi,entityType,barcode,useVerbose=FALSE){
  


  
  
  
  
body<-list(request=list(sdkCmd=jsonlite::unbox("get"),
                             data=list(entityRef=list(name=jsonlite::unbox(""),entitiyID=jsonlite::unbox(""),barcode=jsonlite::unbox(barcode))),
                             responseOptions=c("CONTEXT_GET","MESSAGE_LEVEL_WARN"),
                             typeParam = jsonlite::unbox(entityType),
                             logicOptions=list()
  ))
  
  
  

cookie <-
  c(JSESSIONID = coreApi$jsessionId,
    AWSELB = coreApi$awselb)

  
  
  sdk_url <- paste0(coreApi$scheme,"://",coreApi$coreUrl,"/sdk")
  
  
  response <-httr::POST(sdk_url,body = body, encode="json",
                         httr::set_cookies(cookie),
                         httr::verbose(data_out = useVerbose, data_in = useVerbose,
                         info = useVerbose, ssl = useVerbose)
  )
  
  #check for general HTTP error in response
  
  if(httr::http_error(response)) {
    
    stop(
      {print("API call failed")
        print( httr::http_status(response))
      },
      call.=FALSE
    )
    
    
  }
  
  

  
  list(entity=httr::content(response)$response$data,response=response)
  
}
