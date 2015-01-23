#' Search ThePlantList from local SQL database
#'
#' @export
#'
#' @param genus (character) A genus name, e.g. "Poa"
#' @param species (character) An epithet name, e.g,. "annua"
#' @param family (character) A family name, e.g., "Poaceae"
#' @param authority (character) Taxonomic authority, e.g., "L.".
#' @param status (character) Taxonomic status, one of "Accepted" or ""
#' @param kewid (character) A kew id, e.g., "kew-47692"
#' @param operand (character) One of "AND" or "OR", only used when more than one parameter
#' passed in, combining them. "AND" means both A and B have to be found, whereas "OR" means
#' the search looks for A or B.
#' @param sql A SQL query string. Make sure you include \code{plantlist} as the table name,
#' unless you want to go modify the table name yourself in Postgres to use a different name.
#' See examples for queries. If this parameter is used, the other parameters are ignored.
#'
#' @details \code{\link[taxize]{tpl_search}} does local search of theplantlist SQL data.
#' This function used to wrap the \code{Taxonstand} package, but now does local SQL queries,
#' which are more powerful. If you want \code{Taxonstand} functions, we encourage you to go
#' use that pacakge.
#'
#' \code{\link{tpl_search}} does a limited set of searches, simply searching for matches via
#' \code{variable like 'name'} like queries for variables that you pass in. If you want the
#' full power of SQL, use \code{XXX} and \code{\link{dplyr}} to interact directly with the
#' Thplantlist database.
#'
#' @seealso \code{\link[taxize]{tpl_get}}, \code{\link[taxize]{tpl_families}}, both of which
#' download files from theplantlist.org website
#'
#' @examples \dontrun{
#' backend_get()
#' backend_set("localsql")
#'
#' # pass in parameters to search particular fields
#' tpl_search(genus = "Acanthus")
#' tpl_search(species = "caudatus")
#' tpl_search(genus = "Acanthus", species = "caudatus", operand="AND")
#' tpl_search(genus = "Acanthus", species = "caudatus", operand="OR")
#' tpl_search(kewid = "kew-47692")
#' tpl_search(status = "Accepted")
#'
#' # use raw SQL query
#' ## NOTE: it's important to use 'plantlist' as the table name
#' query <- "SELECT * from plantlist where species like 'annua' limit 5"
#' tpl_search(sql = query)
#'
#' # Or use the SQL database directly with dplyr
#' ## initialize connection to database
#' library("dplyr")
#' tpl_db <- src_postgres(dbname="plantlistdb")
#' ## A sql query
#' tbl(tpl_db, sql("SELECT * from plantlist limit 3")) %>% collect()
#' ## R like query
#' tbl(tpl_db, "plantlist") %>% filter(species == "caudatus")
#' tbl(tpl_db, "plantlist") %>%
#'   filter(genus == "Quercus") %>%
#'   arrange(desc(species))
#' }

tpl_search <- function(genus = NULL, species = NULL, family = NULL, authority = NULL,
  status = NULL, kewid = NULL, operand="AND", sql = NULL)
{
  if(!is.null(sql)){
    query <- sql
  } else {
    args <- taxize_compact(list(genus=genus, species=species, family=family, authorship=authority,
                                taxonomic_status_in_tpl=status, kewid=kewid))
    if(length(args) > 0){
      allargs <- list()
      for(i in seq_along(args)){
        allargs[[i]] <- sprintf("%s like '%s'", names(args[i]), args[[i]])
      }
      if(length(allargs) > 1){
        queryargs <- paste0(allargs, collapse = sprintf(" %s ", operand))
      } else {
        queryargs <- allargs[[1]]
      }
      query <- sprintf("SELECT * from plantlist where %s", queryargs)
    } else {
      stop("No arguments passed :(", .call=FALSE)
    }

  }
  plantlist_SQL(query)
}
