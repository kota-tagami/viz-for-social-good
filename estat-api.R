app_id <- "421ef78c1188ea538a7e3e6371ae7aa7257bb417"
endpoint <- "https://api.e-stat.go.jp/rest/3.0/app/json/"

estat_api <- function(api_method, params = NULL){
  URL <- paste0(endpoint, api_method)
  
  queries <- list(
    "appId" = app_id
  )
  if(!is.null(params)) {
    queries <- c(queries, params)
  }
  
  res <- httr::GET(url = URL, query = queries)
  content <- httr::content(res)
  
  print(httr::http_status(res))
  content
  
}
