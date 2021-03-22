

V1_authBasic <- function(coreApi, useVerbose = FALSE)
  
{
  if (is.null(coreApi$account))
  {
    request <-
      list(request = list(
        data = list(
          lims_userName = jsonlite::unbox(coreApi$user),
          lims_password = jsonlite::unbox(coreApi$pwd)
        ),
        typeParam = jsonlite::unbox("*"),
        sdkCmd = jsonlite::unbox("sdk-login")
      ))
    
  } else  {
    accountObject <-
      list(
        "entityID" = jsonlite::unbox(""),
        "barcode" = jsonlite::unbox(""),
        "name" = jsonlite::unbox(coreApi$account)
      )
    
    request <-
      list(request = list(
        data = list(
          lims_userName = jsonlite::unbox(coreApi$user),
          lims_password = jsonlite::unbox(coreApi$pwd),
          accountRef = accountObject
        ),
        typeParam = jsonlite::unbox("*"),
        sdkCmd = jsonlite::unbox("sdk-login")
      ))
    
  }
  
  
  response <-
    V1_apiPOST(
      coreApi,
      body = request,
      encode = "json",
      useVerbose = useVerbose,
      special = "login"
    )
  
  
  
  getSession <- function(response) {
    jsessionid <-  httr::content(response)$response$data$jsessionid
    awselb <-
      httr::cookies(response)[which(httr::cookies(response)[, 6] == "AWSELB"), 7]
    if (length(awselb) == 0) {
      awselb <- NULL
    }
    employeeId <- httr::content(response)$response$data$employeeId
    list(jsessionid = jsessionid,
         awselb = awselb ,
         employeeId = employeeId)
  }
  
  if (httr::http_error(response)) {
    stop({
      print("API call failed")
      print(httr::http_status(response))
    },
    call. = FALSE)
    
  }
  
  
  session <-
    tryCatch(
      getSession(response),
      error = function(e) {
        list("jsessionid" = NULL, "employeeId" =  NULL)
      }
    )
  
  
  
  if (!is.null(session$jsessionid))
    coreApi$jsessionId <- session$jsessionid
  if (!is.null(session$awselb))
    coreApi$awselb <- session$awselb
  if (!is.null(session$employeeId))
    coreApi$employeeId <- session$employeeId
  
  
  list(coreApi = coreApi, response = response)
  
}
