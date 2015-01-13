#' Download ITIS data
#'
#' @import curl
#' @export
#' @name db
#' @param verbose (logical) Print messages. Default: TRUE
#' @return Downloads sql database, loads it into SQLite, cleans up by removing unneeded files,
#' and gives back path to the database
#' @examples \dontrun{
#' db_itis()
#' }

#' @export
#' @rdname db
db_itis <- function(verbose = TRUE){
  # paths
  itis_db_url <- 'http://www.itis.gov/downloads/itisSqlite.zip'
  itis_db_path <- path.expand('~/.taxize_local/itisSqlite.zip')
  itis_db_path_file <- path.expand('~/.taxize_local/itisSqlite')
  itis_final_file <- path.expand('~/.taxize_local/itis.sql')
  # download data
  mssg(verbose, 'downloading...')
  curl_download(itis_db_url, itis_db_path, quiet=TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  unzip(itis_db_path, exdir = itis_db_path_file)
  # get file path
  dirs <- list.dirs(itis_db_path_file, full.names = TRUE)
  dir_date <- dirs[ dirs != itis_db_path_file ]
  db_path <- list.files(dir_date, pattern = ".sqlite", full.names = TRUE)
  # move database
  file.rename(db_path, itis_final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(itis_db_path)
  unlink(itis_db_path_file, recursive = TRUE)
  # return path
  return( itis_final_file )
}
