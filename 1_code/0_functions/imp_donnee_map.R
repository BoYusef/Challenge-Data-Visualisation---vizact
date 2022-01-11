
### telechargement du fichier map
download_map <- function(url,quiet = FALSE){
  tmpfile <- tempfile(fileext = ".js")
  download.file(url, tmpfile, quiet = quiet)
  mapdata <- readLines(tmpfile, warn = FALSE, encoding = "UTF-8")
  mapdata[1] <- gsub(".* = ", "", mapdata[1])
  mapdata <- paste(mapdata, collapse = "\n")
  mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
  return(mapdata)
}