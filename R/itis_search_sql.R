#' Search ITIS from local SQL database
#'
#' @export
#'
#' @param sql A SQL query string. This database has many tables, so unlike the similar
#' \code{plantlist} function, you have constrct the whole SQL query yourself if you pass
#' query to the \code{sql} parameter.
#'
#' @details \code{\link[taxize]{tpl_search}} does local search of theplantlist SQL data.
#' This function used to wrap the \code{Taxonstand} package, but now does local SQL queries,
#' which are more powerful. If you want \code{Taxonstand} functions, we encourage you to go
#' use that pacakge.
#'
#' Note that you can set the details for your database connection using \code{\link{backend_set}}
#'
#' @examples \dontrun{
#' # use raw SQL query
#' ## NOTE: it's important to use 'plantlist' as the table name
#' query <- "select tsn, rank_id, currency_rating from taxonomic_units where tsn = 28727"
#' itis_search_sql(sql = query)
#'
#' # Or use the SQL database directly with dplyr
#' ## initialize connection to database
#' library("dplyr")
#' itis_db <- src_sqlite(make_path("itis", backend_get()$taxize_path))
#' ## A sql query
#' tbl(itis_db,
#'  sql("select tsn, rank_id, currency_rating from taxonomic_units where tsn = 28727")) %>%
#'  collect()
#'
#' tbl(itis_db,
#'  sql("select * from taxonomic_units limit 1"))
#'
#' ## R like query
#' tbl(itis_db, "taxonomic_units") %>% filter(unit_name1 == "Nitrobacter")
#' }

itis_search_sql <- function(sql) itis_SQL(sql, make_path("itis", backend_get()$taxize_path))
