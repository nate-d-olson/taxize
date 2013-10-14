#' Get the UID codes from NCBI for species names.
#' 
#' Retrieve the Unique Identifier (UID) of a species from NCBI taxonomy browser.
#' 
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers (UID). If a species is not found NA. 
#' If more than one UID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{classification}}
#' 
#' @export
#' @author Eduard Szoecs, \email{szoe8822@@uni-landau.de}
#' 
#' @examples \dontrun{
#' get_uid(c("Chironomus riparius", "Chaetopteryx"))
#' get_uid(c("Chironomus riparius", "aaa vva"))
#' }
get_uid <- function(sciname, verbose = TRUE){
  fun <- function(sciname) {
    if(verbose)
      message("\nRetrieving data for species '", sciname, "'\n")
    sciname <- gsub(" ", "+", sciname)
    searchurl <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy&term=", 
                       sciname, sep = "")
    # NCBI limits requests to three per second
    xml_result <- xmlParse(getURL(searchurl))
    Sys.sleep(0.33)
    id <- xpathSApply(xml_result, "//IdList/Id", xmlValue)    
    # not found on ncbi
    if (length(id) == 0)
      id <- NA
    # more than one found on ncbi -> user input
    if(length(id) > 1){
      baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy"
      ID <- paste("ID=", paste(id, collapse= ","), sep = "")
      searchurl <- paste(baseurl, ID, sep = "&")
      tt <- getURL(searchurl)
      ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
      df <- ldply(xmlToList(ttp), data.frame)
      df <- df[df$Item..attrs != 'String', c(2,5, 7)]
      names(df) <- c("UID", "Rank", "Division")
      rownames(df) <- 1:nrow(df)
      
      # prompt
      message("\n\n")
      message("\nMore than one UID found for species '", sciname, "'!\n
          Enter rownumber of species (other inputs will return 'NA'):\n")      
      print(df)
      take <- scan(n = 1, quiet = TRUE, what = 'raw')
      
      if(length(take) == 0)
        take <- 'notake'
      if(take %in% seq_len(nrow(df))){
        take <- as.numeric(take)
        message("Input accepted, took UID '", as.character(df$UID[take]), "'.\n")
        id <- as.character(df$UID[take])
      } else {
        id <- NA
        message("\nReturned 'NA'!\n\n")
      }
    }  
    return(id)
  }
  out <- laply(sciname, fun)
  class(out) <- "uid"
  return(out)
}