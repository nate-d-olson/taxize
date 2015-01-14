#' ITIS sql query
#'
#' @importFrom dplyr src_sqlite src_postgres src_mysql tbl sql collect %>%
#' @keywords internal
itis_SQL <- function(query, dbpath){
  # initialize connection to database
  itis_db <- src_sqlite(dbpath)
  # query, return data.frame
  tbl(itis_db, sql(query)) %>% collect() %>% data.frame
}

#' COL sql initiation and query
#'
#' @keywords internal
col_SQL <- function(query, user = "root", password = NULL, ...){
  # initialize connection to database
  col_db <- src_mysql(dbname="col", user=user, password=password, ...)
  # query, return data.frame
  tbl(col_db, sql(query)) %>% collect() %>% data.frame
}

#' ThePlantList sql initiation and query
#'
#' @keywords internal
plantlist_SQL <- function(query, user = NULL, password = NULL, ...){
  # initialize connection to database
  tpl_db <- src_postgres(dbname="plantlistdb", user=user, password=password, ...)
  # query, return data.frame
  tbl(tpl_db, sql(query)) %>% collect() %>% data.frame
}
