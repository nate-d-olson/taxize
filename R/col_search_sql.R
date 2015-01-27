#' Search ITIS from local SQL database
#'
#' @export
#'
#' @param sql A SQL query string. This database has many tables, so unlike the similar
#' \code{plantlist} function, you have constrct the whole SQL query yourself if you pass
#' query to the \code{sql} parameter.
#' @param ... Further args passed on to \code{\link{col_SQL}}
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
#' backend_set(col_user="root")
#' query <- "select * from common_name_element where name like '%s beetle'"
#' col_search_sql(query)
#'
#' # Or use the SQL database directly with dplyr
#' ## initialize connection to database
#' library("dplyr")
#' col_db <- src_mysql(dbname="col", user="root")
#' ## A sql query
#' tbl(col_db,
#'  sql("select * from common_name_element where name like '%s beetle'")) %>%
#'  collect()
#'
#' tbl(col_db,
#'  sql("select * from country limit 10"))
#'
#' ## R like query
#' tbl(col_db, "common_name_element") %>% select(name)
#' }

col_search_sql <- function(sql, ...)
  col_SQL(sql, user = backend_get()$col_user, password = backend_get()$col_pwd, ...)
