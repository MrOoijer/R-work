# version 1.1
# change in jsonlite gives arrors
# replace getURL by readLines


library(jsonlite)
# library(RCurl)
##
## call CBS feed or get cached table
##
##
open_feed_api_CBS <- function(table_name, new=FALSE, progress=FALSE){
  #
  api_base = "http://opendata.cbs.nl/ODataFeed/odata/"
  my_cache="./CBSdata/"
  file_name=paste0(my_cache, table_name,sprintf("_%s.dat", format(Sys.time(), "%Y_%b")))
  force_json= "?$format=json"
  #
  if (new == TRUE || ! file.exists(file_name)){
    api_url<-paste0(api_base,table_name, force_json)
    my_data <- readLines(api_url, warn=FALSE)
    my_json <-fromJSON(my_data)
    api_table <-my_json$value
    table_url <- api_table$url[api_table$name=="TypedDataSet"]
    url= paste0(table_url, force_json)
    first=TRUE; iter=1
    while (TRUE){
      # read api interface
      my_data <- readLines(url, warn=FALSE)
      my_json <-fromJSON(my_data)
      if( length(my_json$value) == 0) break
      if( first == FALSE) {data_table<- rbind(data_table, my_json$value)}
      if( first == TRUE) {
        data_table <- my_json$value
        first= FALSE
      }
      if (length(my_json) < 3) break
      url=my_json[[3]]
      if ( progress == TRUE){
        cat(sprintf("\r..%d\r", iter)) ### shows progress during testing 
        iter=iter+1
      }
    }  
    # replace coded fields key with titles
    # ... but use new column name
    #
    for (i in 5:dim(api_table)[1]){
      Naam= api_table$name[i]
      Naam2= paste0(Naam, "_C")
      url= paste0(api_table$url[i], force_json)
      my_data <- readLines(url, warn=FALSE)
      my_json <-fromJSON(my_data)
      key_data <- my_json$value
      for(k in 1:dim(key_data)[1]){
        data_table[data_table[, Naam] == key_data$Key[k], Naam2]<-key_data$Title[k]
      }
    }
    write.table(data_table, file=file_name, row.names=FALSE)
    return(data_table)
  }
  return(read.table(file_name, header=TRUE, stringsAsFactors = FALSE))
}

