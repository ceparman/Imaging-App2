

V1_buildODATAUrl <-
  function(coreApi,
           resource = NULL,
           query = NULL,
           special = NULL,
           useVerbose = FALSE)
  {
    
    #Concat account and odata
    if (!is.null(coreApi$account) && is.null(coreApi$TenantShortName)){
      odat <- paste0("/", V1_ODATAcleanName(coreApi$account), "/odata/")
    }else if (!is.null(coreApi$TenantShortName)){
      odat <- paste0("/", V1_ODATAcleanName(coreApi$TenantShortName), "/odata/")
    }else{ 
      odat <- "/odata/" 
    }
    
    
    if (is.null(special)) {
      sdk_url <-
        paste(
          coreApi$scheme,
          "://",
          coreApi$coreUrl,
          ":",
          coreApi$port,
          odat,
          resource,
          query,
          sep = ""
        )
      
    } else {
      switch(
        special,
        login = sdk_url <-
          paste(
            coreApi$scheme,
            "://",
            coreApi$coreUrl,
            ":",
            coreApi$port,
            "/odatalogin",
            sep = ""
          ),
        file = sdk_url <-
          paste0(
            coreApi$scheme,
            "://",
            coreApi$coreUrl,
            ":",
            coreApi$port,
            "/sdk"
          ),
        json = sdk_url <-
          paste0(
            coreApi$scheme,
            "://",
            coreApi$coreUrl,
            ":",
            coreApi$port,
            "/sdk"
          )
        
        
      )
    } 
    
    return(sdk_url)
    
  }