


V1_apiPOST <-
  function(coreApi,
           resource = NULL,
           body = NULL,
           encode,
           headers = NULL,
           special = NULL,
           useVerbose = FALSE)
  {
    #clean the resource name for ODATA
    
    resource <- V1_ODATAcleanName(resource)
    
    
    #Check that encode parameter is proper
    
    if (!(encode %in% c("multipart", "form", "json", "raw"))) {
      stop({
        print("encode parameter not recognized")
        print(httr::http_status(response))
      },
      call. = FALSE)
      
    }
    
    
    
    sdk_url <-
      V1_buildODATAUrl(
        coreApi,
        resource = resource,
        special = special,
        useVerbose = useVerbose
      )
    
    cookie <-
      c(JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb)
    
    response <-
      invisible(
        httr::POST(
          sdk_url,
          resource = resource,
          body = body,
          encode = encode,
          httr::add_headers(headers),
          httr::set_cookies(cookie),
          httr::verbose(
            data_out = useVerbose,
            data_in = useVerbose,
            info = useVerbose,
            ssl = useVerbose
          )
        )
      )
    
    
    
    
    
    
    #check for general HTTP error in response
    
    if (httr::http_error(response)) {
      stop({
        print("API call failed")
        print(httr::http_status(response))
      },
      call. = FALSE)
      
      
    }
    
    return(response)
  }