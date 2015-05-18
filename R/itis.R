itbase <- function() 'http://www.itis.gov/ITISWebService/services/ITISService/'

itis_GET <- function(endpt, args, ...){
  if (length(args) == 0) args <- NULL
  tt <- GET(paste0(itbase(), endpt), query = args, ...)
  xmlParse(content(tt, "text"))
}

itis_parse <- function(a, b, d){
  xpathfunc <- function(x, y, nsp) {
    sapply(getNodeSet(y, paste("//ax21:", x, sep = ''), namespaces = nsp), xmlValue)
  }
  df <- setNames(data.frame(t(sapply(a, xpathfunc, y = b, nsp = d))), a)
  colClasses(df, "character")
}

itisdf <- function(a, b, matches, colnames, pastens="ax21"){
  matches <- paste0(sprintf('//%s:', pastens), matches)
  output <- c()
  for (i in seq_along(matches)) {
    nodes <- getNodeSet(a, matches[[i]], namespaces = b)
    output[[i]] <- sapply(nodes, xmlValue)
  }
  if (all(sapply(output, length) == 1))
    setNames(data.frame(t(output), stringsAsFactors = FALSE), colnames)
  else
    setNames(data.frame(output), colnames)
}

#' Get accepted names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' # TSN accepted - good name
#' library('httr')
#' getacceptednamesfromtsn('208527', config=timeout(1))
#'
#' # TSN not accepted - input TSN is old name
#' getacceptednamesfromtsn('504239', config=timeout(1))
#'
#' # TSN not accepted - input TSN is old name
#' getacceptednamesfromtsn('504239', config=timeout(3))
#' }
#' @export
#' @keywords internal
getacceptednamesfromtsn <- function(tsn, ...) {
  tt_ <- itis_GET("getAcceptedNamesFromTSN", list(tsn = tsn), ...)
	temp <- xmlToList(tt_)
	if (length(temp$return$acceptedNames) == 1) {
    temp$return$tsn
  } else {
		c(submittedTsn = temp$return$tsn, temp$return$acceptedNames[1:2])
	}
}

#' Get any match count.
#'
#' @param x text or taxonomic serial number (TSN) (character or numeric)
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... optional additional curl options (debugging tools mostly)
#' @return An integer containing the number of matches the search will return.
#' @examples \dontrun{
#' library('httr')
#' getanymatchcount(202385, config=timeout(3))
#' getanymatchcount("dolphin", config=timeout(3))
#'
#' # local sql
#' backend_set("local")
#' getanymatchcount(x=202385)
#' getanymatchcount(x="dolphin")
#' }
#' @export
#' @keywords internal
getanymatchcount <- function(x, backend=NULL, ...) {
  backend <- get_back(backend)
	if( backend == "local" ) {
	  query <- if (is.numeric(x)) {
	    paste("Select count(*) from taxonomic_units where tsn = ", x)
	  } else {
	    paste("Select count(*)
                from taxonomic_units t
                inner join vernaculars v on v.tsn = t.tsn and v.vernacular_name like ", paste("'", x, "'", sep = ""), "union
                    Select count(*)
                    from taxonomic_units t
                    where t.complete_name like ", paste("'", x, "'", sep = ""))
	  }
	  itis_SQL(query)[[1]]
	} else {
	  out <- itis_GET("getAnyMatchCount", list(srchKey = x), ...)
	  as.numeric(xmlToList(out)$return)
	}
}

#' Get comment detail from TSN
#'
#' @param tsn TSN for a taxonomic group (numeric)
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... optional additional curl options (debugging tools mostly)
#' @return A data.frame with results.
#' @examples \dontrun{
#' library("httr")
#' getcommentdetailfromtsn(tsn=180543, config=timeout(4))
#'
#' # using sql locally
#' backend_get()
#' backend_set("local")
#' getcommentdetailfromtsn(tsn=180543)
#' }
#' @export
#' @keywords internal
getcommentdetailfromtsn <- function(tsn, backend=NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select c.* from comments c inner join tu_comments_links t
              on c.comment_id = t.comment_id and tsn = ", tsn, "order by comment_time_stamp")
    itis_SQL(query)
  } else {
    out <- itis_GET("getCommentDetailFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    matches <- c("commentDetail", "commentId", "commentTimeStamp", "commentator","updateDate")
    colnames <- c('comment','commid','commtime','commentator','updatedate')
    itisdf(a = out, b = namespaces, matches = matches, colnames = colnames)
  }
}

get_back <- function(x){
  if( is.null(x) ){
    bb <- backend_get()
    bb$itis_backend
  } else {
    mb(x)
  }
}

#' Get common names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcommonnamesfromtsn(183833, config=timeout(1))
#'
#' # local
#' getcommonnamesfromtsn(183833, backend="local")
#' }
#' @export
#' @keywords internal
getcommonnamesfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select t.tsn as tsn, v.language as language, a.taxon_author as author,
          v.vernacular_name as commonName, t.complete_name as combinedName
          from vernaculars v
          inner join taxonomic_units t on v.tsn = t.tsn
          and t.tsn = ", tsn, "left join taxon_authors_lkp a on a.taxon_author_id = t.taxon_author_id")
    itis_SQL(query)
  } else {
    out <- itis_GET("getCommonNamesFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
    comname <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
    lang <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
    tsn <- sapply(nodes, xmlValue)
    data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)])
  }
}

#' Get core metadata from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' library("httr")
#' getcoremetadatafromtsn(28727, config=timeout(3))  # coverage and currrency data
#' getcoremetadatafromtsn(183671, config=timeout(4))  # no coverage or currrency data
#'
#' backend_set("local")
#' getcoremetadatafromtsn(tsn=183671)
#' }
#' @export
#' @keywords internal
getcoremetadatafromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select tsn, rank_id, name_usage, unaccept_reason, credibility_rtng,
        completeness_rtng, currency_rating from taxonomic_units where tsn = ", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getCoreMetadataFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("credRating","rankId","taxonCoverage","taxonCurrency","taxonUsageRating","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Get coverge from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcoveragefromtsn(tsn=28727, config=timeout(4))  # coverage data
#' getcoveragefromtsn(526852, config=timeout(4))  # no coverage data
#'
#' backend_set("local")
#' getcoveragefromtsn(tsn=28727)
#' }
#' @export
#' @keywords internal
getcoveragefromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select tsn, rank_id, completeness_rtng from taxonomic_units where tsn =", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getCoverageFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    matches <- c("rankId", "taxonCoverage", "tsn")
    itisdf(a=out, b=namespaces, matches, colnames = tolower(matches))
  }
}

#' Get credibility rating from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcredibilityratingfromtsn(526852, config=timeout(4))
#'
#' backend_set("local")
#' getcredibilityratingfromtsn(526852)
#' }
#' @export
#' @keywords internal
getcredibilityratingfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select tsn, credibility_rtng from taxonomic_units where tsn =", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getCredibilityRatingFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    matches <- c("credRating", "tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Get possible credibility ratings
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getcredibilityratings(config=timeout(3))
#'
#' backend_set("local")
#' getcredibilityratings()
#' }
#' @export
#' @keywords internal
getcredibilityratings <- function(backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select distinct credibility_rtng from taxonomic_units order by credibility_rtng")
    itis_SQL(query)
  } else {
    out <- itis_GET("getCredibilityRatings", list(), ...)
    namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
    nodes <- getNodeSet(out, "//ax23:credibilityValues", namespaces=namespaces)
    credibilityValues <- sapply(nodes, xmlValue)
    data.frame(credibilityValues = credibilityValues, stringsAsFactors = FALSE)
  }
}

#' Get currency from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcurrencyfromtsn(28727, config=timeout(3)) # currency data
#' getcurrencyfromtsn(526852, config=timeout(3)) # no currency dat
#'
#' backend_set("local")
#' getcurrencyfromtsn(28727)
#' }
#' @export
#' @keywords internal
getcurrencyfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select tsn, rank_id, currency_rating from taxonomic_units where tsn =", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getCurrencyFromTSN", list(tsn = tsn), ...)
    namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
    matches <- c("rankId","taxonCurrency","tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Get date data from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getdatedatafromtsn(180543, config=timeout(3))
#'
#' backend_set("local")
#' getdatedatafromtsn(180543)
#' }
#' @export
#' @keywords internal
getdatedatafromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select initial_time_stamp, update_date from taxonomic_units where tsn = ", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getDateDataFromTSN", list(tsn = tsn), ...)
    namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
    matches <- c("initialTimeStamp","updateDate","tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Get description of the ITIS service
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @details There is no SQL option for this function.
#' @examples \dontrun{
#' getdescription(config=timeout(1))
#' }
#' @export
#' @keywords internal
getdescription <- function(...){
  getNodeSet(itis_GET("getDescription", list(), ...), "//ns:return", fun=xmlValue)[[1]]
}

#' Get expert information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getexpertsfromtsn(180544, "api")
#'
#' backend_set("local")
#' getexpertsfromtsn(180544)
#' }
#' @export
#' @keywords internal
getexpertsfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select '1' as sort_order, r.vernacular_name, NULL As language, e.*
        from reference_links r, experts e
        where r.doc_id_prefix = e.expert_id_prefix and r.documentation_id = e.expert_id
        and (r.vernacular_name = '' or r.vernacular_name is null) and r.tsn = ", tsn, "UNION Select '2' as sort_order, v.vernacular_name, v.language, e.*
          from vernaculars v, experts e, vern_ref_links vr
          where vr.doc_id_prefix = e.expert_id_prefix and vr.documentation_id = e.expert_id
          and vr.vern_id = v.vern_id and vr.tsn = v.tsn and v.tsn = ", tsn, "order by expert, sort_order")
    itis_SQL(query)
  } else {
    out <- itis_GET("getExpertsFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("comment","expert","name","referredTsn","referenceFor","updateDate")
    xpathfunc <- function(x) {
      sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
    }
    setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget)
  }
}

#' Get full hierarchy from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @details No local SQL method yet - ITIS uses a complicated Java program on the server
#' side to do this, maybe later we'll have this local. We're using SQLite right now for ITIS
#' data, but when moved to PostgreSQL we can maybe make this work locally.
#' @examples \dontrun{
#' getfullhierarchyfromtsn(37906, config=timeout(3))
#' getfullhierarchyfromtsn(100800, config=timeout(3))
#' }
#' @export
#' @keywords internal

getfullhierarchyfromtsn <- function(tsn, ...) {
	out <- itis_GET("getFullHierarchyFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:parentName", namespaces = namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:parentTsn", namespaces = namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:rankName", namespaces = namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonName", namespaces = namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  tsn <- sapply(nodes, xmlValue)
	data.frame(parentName = parentName, parentTsn = parentTsn,
	           rankName = rankName[-length(rankName)],
	           taxonName = taxonName, tsn = tsn[-1], stringsAsFactors = FALSE)
}

#' Returns the full ITIS record for the TSN in the LSID, found by comparing the
#' 		TSN in the search key to the TSN field. Returns an empty result set if
#'   	there is no match or the TSN is invalid.
#'
#' @param lsid lsid for a taxonomic group (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @details No local SQL method yet - will try to get this sorted soon.
#' @examples \dontrun{
#' getfullrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' getfullrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' }
#' @export
#' @keywords internal
getfullrecordfromlsid <- function(lsid, ...) {
	out <- itis_GET("getFullRecordFromLSID", list(lsid = lsid), ...)
	namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
	toget <- c("acceptedNameList","commentList","commonNameList","completenessRating",
	           "coreMetadata","credibilityRating","currencyRating","dateData","expertList",
	           "geographicDivisionList","hierarchyUp","jurisdictionalOriginList",
	           "kingdom","otherSourceList","parentTSN","publicationList","scientificName",
	           "synonymList","taxRank","taxonAuthor","unacceptReason","usage")
	parsedat <- function(x){
	  tmp <- getNodeSet(out, sprintf("//ax21:%s",x), namespaces = namespaces, xmlToList)[[1]]
	  tmp[!names(tmp) %in% ".attrs"]
	}
	setNames(lapply(toget, parsedat), toget)
}

#' Get full record from TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @details No local SQL method yet - will try to get this sorted soon.
#' @examples \dontrun{
#' getfullrecordfromtsn(504239, config=timeout(3))
#' getfullrecordfromtsn(202385, config=timeout(3))
#' getfullrecordfromtsn(183833, config=timeout(3))
#' }
#' @export
#' @keywords internal
getfullrecordfromtsn <- function(tsn, ...) {
	out <- itis_GET("getFullRecordFromTSN", list(tsn = tsn), ...)
	namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
  toget <- c("acceptedNameList","commentList","commonNameList","completenessRating",
             "coreMetadata","credibilityRating","currencyRating","dateData","expertList",
             "geographicDivisionList","hierarchyUp","jurisdictionalOriginList",
             "kingdom","otherSourceList","parentTSN","publicationList","scientificName",
             "synonymList","taxRank","taxonAuthor","unacceptReason","usage")
	parsedat <- function(x){
	  tmp <- getNodeSet(out, sprintf("//ax21:%s",x), namespaces = namespaces, xmlToList)[[1]]
    tmp[!names(tmp) %in% ".attrs"]
	}
  setNames(lapply(toget, parsedat), toget)
}

#' Get geographic divisions from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getgeographicdivisionsfromtsn(180543, "api")
#'
#' backend_set("local")
#' getgeographicdivisionsfromtsn(180543)
#' }
#' @export
#' @keywords internal
getgeographicdivisionsfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select * from geographic_div where tsn = ", tsn, "order by geographic_value")
    itis_SQL(query)
  } else {
    out <- itis_GET("getGeographicDivisionsFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("geographicValue","updateDate")
    itis_parse(toget, out, namespaces)
  }
}

#' Get all possible geographic values
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getgeographicvalues("api")
#'
#' backend_set("local")
#' getgeographicvalues()
#' }
#' @export
#' @keywords internal
getgeographicvalues <- function(backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select distinct geographic_value from geographic_div order by geographic_value")
    itis_SQL(query)
  } else {
    out <- itis_GET("getGeographicValues", list(), ...)
    namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.gov/xsd" )
    nodes <- getNodeSet(out, "//ax21:geographicValues", namespaces=namespaces)
    geographicValues <- sapply(nodes, xmlValue)
    data.frame(geographicValues = geographicValues, stringsAsFactors=FALSE)
  }
}

#' Get global species completeness from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getglobalspeciescompletenessfromtsn(180541, "api")
#'
#' backend_set("local")
#' getglobalspeciescompletenessfromtsn(180541)
#' }
#' @export
#' @keywords internal
getglobalspeciescompletenessfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select tsn, rank_id, completeness_rtng from taxonomic_units where tsn =", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getGlobalSpeciesCompletenessFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("completeness","rankId","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Get hierarchy down from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gethierarchydownfromtsn(161030, "api")
#'
#' backend_set("local")
#' gethierarchydownfromtsn(161030)
#' }
#' @export
#' @keywords internal
gethierarchydownfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select t.tsn, t.parent_tsn, t.complete_name as combinedName,
         r.rank_name, r.rank_id, a.taxon_author as author
         from taxonomic_units t
         left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
         inner join taxon_unit_types r on r.rank_id = t.rank_id and r.kingdom_id = t.kingdom_id
         where (t.parent_tsn= ", tsn, " or t.tsn= ", tsn, ")
            and (t.name_usage='valid' or t.name_usage='accepted')")
    temp <- itis_SQL(query)
    temp2 <- data.frame(parentTsn = temp$parent_tsn, rankName = temp$rank_name, rankId = temp$rank_id,
                        taxonName = temp$combinedname, tsn = temp$tsn, stringsAsFactors = FALSE)
    temp2[!temp2$tsn %in% tsn,]
  } else {
    out <- itis_GET("getHierarchyDownFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    matches <- paste0("hierarchyList/ax21:",
                      c("parentName","parentTsn","rankName","taxonName","tsn"))
    itisdf(out, namespaces, matches, tolower(c("parentName","parentTsn","rankName","taxonName","tsn")))
  }
}

#' Get hierarchy up from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gethierarchyupfromtsn(36485, "api")
#'
#' backend_set("local")
#' gethierarchyupfromtsn(36485)
#' }
#' @export
#' @keywords internal
gethierarchyupfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select distinct t.parent_tsn, t.tsn, l.complete_name as parent_name,
         a.taxon_author as author, t.complete_name as combinedName, r.rank_name
         from taxonomic_units t
         left outer join Taxonomic_units l  on l.tsn = t.parent_tsn
         left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
         inner join taxon_unit_types r on r.rank_id = t.rank_id and r.kingdom_id = t.kingdom_id
         where t.tsn=", tsn)
    temp <- itis_SQL(query)
    data.frame(parentName = temp$parent_name, parentTsn = temp$parent_tsn,
               rankName = temp$rank_name, taxonName = temp$combinedname,
               tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getHierarchyUpFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    matches <- c("parentName","parentTsn","rankName","taxonName","tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Get itis terms from common names
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' getitistermsfromcommonname("buya", "api")
#'
#' library('httr')
#' getitistermsfromcommonname("buya", "api", config=timeout(1))
#'
#' backend_set("local")
#' getitistermsfromcommonname(x="buya")
#' }
#' @export
#' @keywords internal
getitistermsfromcommonname <- function(x, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- sprintf("select t.tsn, t.name_usage, t.complete_name as combinedName, v.vernacular_name, a.taxon_author as author
        from taxonomic_units t
        left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
        inner join vernaculars v on v.tsn = t.tsn and v.vernacular_name like '%s' order by t.tsn", x)
    itis_SQL(query)
  } else {
    out <- itis_GET("getITISTermsFromCommonName", list(srchKey = x), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    gg <- getNodeSet(out, "//ax21:itisTerms", namespaces=namespaces, xmlToList)
    tmp <- do.call(rbind.fill, lapply(gg, function(x) data.frame(x,stringsAsFactors=FALSE)))
    names(tmp) <- tolower(names(tmp))
    row.names(tmp) <- NULL
    if(nrow(tmp)==1 && names(tmp)=="x"){
      NA
    } else {
      tmp$commonnames <- gsub("true", NA, as.character(tmp$commonnames))
      tmp$.attrs <- as.character(tmp$.attrs)
      tmp
    }
  }
}

#' Get itis terms from either common or scientific names
#'
#' @inheritParams getanymatchcount
#' @details No local SQL method.
#' @examples \dontrun{
#' getitisterms("bear")
#' }
#' @export
#' @keywords internal
getitisterms <- function(x, ...) {
  out <- itis_GET("getITISTerms", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  gg <- getNodeSet(out, "//ax21:itisTerms", namespaces = namespaces, xmlToList)
  tmp <- do.call(rbind.fill, lapply(gg, function(x) data.frame(x,stringsAsFactors=FALSE)))
  names(tmp) <- tolower(names(tmp))
  row.names(tmp) <- NULL
  if(nrow(tmp)==1 && names(tmp)=="x"){
    NA
  } else {
    tmp$commonnames <- gsub("true", NA, as.character(tmp$commonnames))
    tmp$.attrs <- as.character(tmp$.attrs)
    tmp
  }
}

#' Get itis terms from scientific names
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' getitistermsfromscientificname("ursidae")
#' getitistermsfromscientificname("Ursus")
#'
#' backend_set("local")
#' getitistermsfromscientificname("ursidae")
#' getitistermsfromscientificname("Ursus")
#' getitistermsfromscientificname("Poa")
#' }
#' @export
#' @keywords internal
getitistermsfromscientificname <- function(x, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- sprintf("select t.tsn, t.name_usage, t.complete_name as combinedName, v.vernacular_name, a.taxon_author as author
                        from taxonomic_units t
                        left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
                        left outer join vernaculars v on v.tsn = t.tsn
                        where t.complete_name like '%s' order by t.tsn", x)
    itis_SQL(query)
  } else {
    out <- itis_GET("getITISTermsFromScientificName", list(srchKey = x), ...)
    namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
    gg <- getNodeSet(out, "//ax21:itisTerms", namespaces = namespaces,
                     xmlToList)
    tmp <- do.call(rbind.fill, lapply(gg, function(x) data.frame(x,
                                                                 stringsAsFactors = FALSE)))
    names(tmp) <- tolower(names(tmp))
    row.names(tmp) <- NULL
    if (nrow(tmp) == 1 && names(tmp) == "x") {
      NA
    }
    else {
      tmp$commonnames <- gsub("true", NA, as.character(tmp$commonnames))
      tmp$.attrs <- as.character(tmp$.attrs)
      tmp
    }
  }
}

#' Get jurisdictional origin from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getjurisdictionaloriginfromtsn(180543, "api")
#'
#' backend_set("local")
#' getjurisdictionaloriginfromtsn(180543)
#' }
#' @export
#' @keywords internal
getjurisdictionaloriginfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- sprintf("Select * from jurisdiction where tsn = %s order by jurisdiction_value", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getJurisdictionalOriginFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("jurisdictionValue","origin","updateDate")
    xpathfunc <- function(x) {
      sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
    }
    df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
    if(nrow(df) == 0){
      data.frame(jurisdictionvalue=NA,origin=NA,updatedate=NA)
    } else {
      setNames(df, tolower(toget))
    }
  }
}

#' Get jurisdiction origin values
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getjurisdictionoriginvalues("api")
#'
#' backend_set("local")
#' getjurisdictionoriginvalues()
#' }
#' @export
#' @keywords internal
getjurisdictionoriginvalues <- function(backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- "select distinct jurisdiction_value, origin from jurisdiction order by jurisdiction_value, origin"
    itis_SQL(query)
  } else {
    out <- itis_GET("getJurisdictionalOriginValues", list(), ...)
    namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
    matches <- c("jurisdiction","origin")
    itisdf(a=out, b=namespaces, matches=matches, colnames=tolower(matches), pastens="ax23")
  }
}

#' Get possible jurisdiction values
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getjurisdictionvalues(config=timeout(3))
#'
#' backend_set("local")
#' getjurisdictionvalues()
#' }
#' @export
#' @keywords internal
getjurisdictionvalues <- function(backend = NULL, ...)
{
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- "select distinct jurisdiction_value from jurisdiction order by jurisdiction_value"
    itis_SQL(query)
  } else {
    out <- itis_GET("getJurisdictionValues", list(), ...)
    namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
    nodes <- getNodeSet(out, "//ax23:jurisdictionValues", namespaces=namespaces)
    jurisdictionValues <- sapply(nodes, xmlValue)
    data.frame(jurisdictionValues = jurisdictionValues, stringsAsFactors = FALSE)
  }
}

#' Get kingdom names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getkingdomnamefromtsn(202385, config=timeout(3))
#'
#' backend_set("local")
#' getkingdomnamefromtsn(202385)
#' }
#' @export
#' @keywords internal
getkingdomnamefromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- sprintf("SELECT kingdom_name as KingdomName, kingdom_id as KingdomID from kingdoms where kingdom_id=(select kingdom_id from taxonomic_units where tsn = %s)", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getKingdomNameFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("kingdomId","kingdomName","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Get all possible kingdom names
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getkingdomnames(config=timeout(3))
#'
#' backend_set("local")
#' getkingdomnames()
#' }
#' @export
#' @keywords internal
getkingdomnames <- function(backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select distinct k.*, t.tsn
                  from kingdoms k
                  inner join taxonomic_units t on t.unit_name1 = k.kingdom_name and t.parent_tsn=0
                  order by k.kingdom_id")
    itis_SQL(query)
  } else {
    out <- itis_GET("getKingdomNames", list(), ...)
    namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.gov/xsd")
    matches <- c("kingdomId","kingdomName","tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Provides the date the ITIS database was last updated.
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getlastchangedate("api")
#' getlastchangedate("local")
#' }
#' @export
#' @keywords internal
getlastchangedate <- function(backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select max(update_date) from taxonomic_units")
    itis_SQL(query)
  } else {
    out <- itis_GET("getLastChangeDate", list(), ...)
    namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.gov/xsd")
    nodes <- getNodeSet(out, "//ax21:updateDate", namespaces=namespaces)
    sapply(nodes, xmlValue)
  }
}

#' Gets the unique LSID for the TSN, or an empty result if there is no match.
#'
#' @inheritParams getcommentdetailfromtsn
#' @details No local SQL method.
#' @examples \dontrun{
#' getlsidfromtsn(155166, config=timeout(3))
#' }
#' @export
#' @keywords internal
getlsidfromtsn <- function(tsn, ...) xmlToList(itis_GET("getLSIDFromTSN", list(tsn = tsn), ...))[[1]]

#' Returns a list of the other sources used for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getothersourcesfromtsn(182662, config=timeout(3))
#' getothersourcesfromtsn(182662, "api")
#' getothersourcesfromtsn(182662, "local")
#' }
#' @export
#' @keywords internal
getothersourcesfromtsn <- function(tsn, backend=NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select '1' as sort_order, r.original_desc_ind, NULL as language, r.vernacular_name, o.*
         from reference_links r, other_sources o where r.doc_id_prefix = o.source_id_prefix
         and r.documentation_id = o.source_id and (r.vernacular_name = '' or r.vernacular_name is null) and r.tsn = ", tsn, "UNION Select '2' as sort_order,'N' AS original_desc_ind, v.language, v.vernacular_name, o.*
          from vern_ref_links vr, other_sources o, vernaculars v
          where vr.doc_id_prefix = o.source_id_prefix and vr.documentation_id = o.source_id
          and vr.vern_id = v.vern_id and vr.tsn = v.tsn and v.tsn = ", tsn, "order by source, version, sort_order")
    itis_SQL(query)
  } else {
    out <- itis_GET("getOtherSourcesFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("acquisitionDate","name","referredTsn","source",
                  "sourceType","updateDate","version")
    xpathfunc <- function(x) {
      sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
    }
    setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget)
  }
}

#' Returns the parent TSN for the entered TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getparenttsnfromtsn(202385, config=timeout(3))
#' getparenttsnfromtsn(202385, "api")
#' getparenttsnfromtsn(202385, "local")
#' }
#' @export
#' @keywords internal
getparenttsnfromtsn <- function(tsn, backend=NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT parent_tsn as ParentTSN from taxonomic_units WHERE",
         paste(sapply(tsn, function(x) paste("tsn =", x, sep=" "), USE.NAMES=FALSE), collapse = " OR "))
    temp <- itis_SQL(query)
    setNames(cbind(temp, tsn), c("parentTsn", "tsn"))
  } else {
    out <- itis_GET("getParentTSNFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("parentTsn","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Returns a list of the pulications used for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getpublicationsfromtsn(70340, config=timeout(3))
#' getpublicationsfromtsn(70340, "api")
#' getpublicationsfromtsn(70340, "local")
#' }
#' @export
#' @keywords internal
getpublicationsfromtsn <- function(tsn, backend=NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("Select '1' as sort_order, r.vernacular_name, NULL as language, r.original_desc_ind, p.*
        from reference_links r, publications p
        where r.doc_id_prefix = p.pub_id_prefix and r.documentation_id = p.publication_id
        and (r.vernacular_name ='' or r.vernacular_name is null) and tsn
        UNION Select '2' as sort_order, v.vernacular_name, v.language, 'N' as original_desc_ind, p.*
        From vern_ref_links vr, publications p, vernaculars v
        where vr.doc_id_prefix = p.pub_id_prefix and vr.documentation_id = p.publication_id and vr.vern_id = v.vern_id
        and vr.tsn = v.tsn and vr.tsn = ", tsn, "order by reference_author, actual_pub_date, title, publication_name, sort_order")
    itis_SQL(query)
  } else {
    out <- itis_GET("getPublicationsFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("actualPubDate","isbn","issn","listedPubDate","pages",
                  "pubComment","pubName","pubPlace","publisher","referenceAuthor",
                  "name","refLanguage","referredTsn","title","updateDate")
    xpathfunc <- function(x) {
      sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
    }
    df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
    if(NROW(df) > 0) names(df) <- tolower(toget)
    df
  }
}

#' Provides a list of all the unique rank names contained in the database and
#'  their kingdom and rank ID values.
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getranknames("api")
#' getranknames("local")
#' }
#' @export
#' @keywords internal
getranknames <- function(backend=NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select k.kingdom_name, t.kingdom_id, t.rank_name, t.rank_id
               from taxon_unit_types t
                inner join kingdoms k on t.kingdom_id = k.kingdom_id
                order by t.kingdom_id, t.rank_id")
    itis_SQL(query)
  } else {
    out <- itis_GET("getRankNames", list(), ...)
    namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
    matches <- c("kingdomName","rankId","rankName")
    itisdf(out, namespaces, matches, tolower(matches), "ax23")
  }
}

#' Gets the partial ITIS record for the TSN in the LSID, found by comparing the
#'  TSN in the search key to the TSN field. Returns an empty result set if
#'  there is no match or the TSN is invalid.
#'
#' @inheritParams getfullrecordfromlsid
#' @details No local SQL method.
#' @examples \dontrun{
#' getrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' }
#' @export
#' @keywords internal
getrecordfromlsid <- function(lsid, ...) {
	out <- itis_GET("getRecordFromLSID", list(lsid = lsid), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("authorship","genusPart","infragenericEpithet",
                "infraspecificEpithet","lsid","nameComplete","nomenclaturalCode",
                "rank","rankString","specificEpithet","uninomial","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns the review year for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getreviewyearfromtsn(180541, "api")
#' getreviewyearfromtsn(180541, "local")
#' }
#' @export
#' @keywords internal
getreviewyearfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste0("Select tsn, rank_id, currency_rating from taxonomic_units where tsn = ", tsn)
    itis_SQL(query)
  } else {
    out <- itis_GET("getReviewYearFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("rankId","reviewYear","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Returns the scientific name for the TSN. Also returns the component parts
#'    (names and indicators) of the scientific name.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getscientificnamefromtsn(531894, "api")
#' getscientificnamefromtsn(531894, "local")
#' }
#' @export
#' @keywords internal
getscientificnamefromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select t.tsn, t.unit_ind1, t.unit_name1, t.unit_ind2, t.unit_name2,
        t.unit_ind3, t.unit_name3, t.unit_ind4, t.unit_name4,
        t.complete_name as combinedName, a.taxon_author as author, k.kingdom_name as kingdom
        from taxonomic_units t
        join kingdoms k on t.kingdom_id = k.kingdom_id
        left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
        WHERE", paste0(sapply(tsn, function(x) paste("t.tsn = ", x, sep = ""), USE.NAMES=FALSE), collapse=" OR "))
    temp <- itis_SQL(query)
    temp <- setNames(temp[, c("tsn", "unit_ind1", "unit_name1", "unit_name2",
                     "unit_ind3", "unit_name3", "combinedName")], c("tsn", "unitInd1", "unitName1", "unitName2",
                                                                    "unitInd3", "unitName3", "combinedName"))
    temp[, c("combinedName", "unitInd1", "unitInd3", "unitName1", "unitName2", "unitName3", "tsn")]
  } else {
    out <- itis_GET("getScientificNameFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("combinedName","unitInd1","unitInd3","unitName1","unitName2",
                  "unitName3","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Returns a list of the synonyms (if any) for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getsynonymnamesfromtsn(183671, "api") # tsn not accepted
#' getsynonymnamesfromtsn(526852, "api") # tsn accepted
#'
#' getsynonymnamesfromtsn(183671, "local") # tsn not accepted
#' getsynonymnamesfromtsn(526852, "local") # tsn accepted
#' }
#' @export
#' @keywords internal
getsynonymnamesfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT",
         paste("CASE", paste(sapply(tsn, function(x) paste("WHEN s.tsn_accepted = ", x, " THEN ", x, sep = ""), USE.NAMES=FALSE), collapse=" "), "END AS querystring,"),
         "t.tsn, t.complete_name as combinedName, a.taxon_author as author
         from taxonomic_units t
         left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
         inner join synonym_links s on t.tsn = s.tsn WHERE ",
         paste0(sapply(tsn, function(x) paste("s.tsn_accepted = ", x, sep = ""), USE.NAMES=FALSE), collapse=" OR "), " order by querystring")
    temp <- itis_SQL(query)
    data.frame(name = temp$combinedName, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getSynonymNamesFromTSN", list(tsn = tsn), ...)
    namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
    nodes <- getNodeSet(out, "//ax21:sciName", namespaces=namespaces)
    if( length(sapply(nodes, xmlValue)) == 0){ name <- list("nomatch") } else
    { name <- sapply(nodes, xmlValue) }
    nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
    if( length(sapply(nodes, xmlValue)) == 1){ tsn <- sapply(nodes, xmlValue) } else
    {
      tsn <- sapply(nodes, xmlValue)
      tsn <- tsn[-1]
    }
    data.frame(name=name, tsn=tsn, stringsAsFactors = FALSE)
  }
}

#' Returns the author information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @details When \code{backend="local"} you can pass in many TSN's in a vector, but only one
#' when \code{backend="api"}
#' @examples \dontrun{
#' gettaxonauthorshipfromtsn(183671, "api")
#' gettaxonauthorshipfromtsn(183671, "local")
#' gettaxonauthorshipfromtsn(tsn=c(202385,531894,526852,183671), "local")
#' }
#' @export
#' @keywords internal
gettaxonauthorshipfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select t.tsn as tsn, a.taxon_author as author, a.update_date as date
        from taxonomic_units t
        inner join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id WHERE",
          paste0(sapply(tsn, function(x) paste("t.tsn = ", x, sep = ""), USE.NAMES=FALSE), collapse=" OR "))
    itis_SQL(query)
  } else {
    out <- itis_GET("getTaxonAuthorshipFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("authorship","updateDate","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Returns the kingdom and rank information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonomicranknamefromtsn(202385, "api")
#' gettaxonomicranknamefromtsn(202385, "local")
#' }
#' @export
#' @keywords internal
gettaxonomicranknamefromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT t.kingdom_id, t.rank_id, t.tsn, r.rank_name, k.kingdom_name from taxonomic_units t
        inner join taxon_unit_types r on r.rank_id = t.rank_id
        inner join kingdoms k on k.kingdom_id = t.kingdom_id WHERE", paste0(sapply(tsn, function(x) paste("t.tsn = ", x, sep = ""), USE.NAMES=FALSE), collapse=" OR "))
    temp <- unique(itis_SQL(query))
    data.frame(kingdomId = temp$kingdom_id, kingdomName = temp$kingdom_name,
               rankId = temp$rank_id, rankName = temp$rank_name, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getTaxonomicRankNameFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("kingdomId","kingdomName","rankId","rankName","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Returns the usage information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonomicusagefromtsn(526852, "api")
#' gettaxonomicusagefromtsn(526852, "local")
#' }
#' @export
#' @keywords internal
gettaxonomicusagefromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select tsn, name_usage from taxonomic_units where ",
                   paste0(sapply(tsn, function(x) paste("tsn = ", x, sep = ""), USE.NAMES=FALSE), collapse=" OR "))
    temp <- itis_SQL(query)
    data.frame(taxonUsageRating = temp$name_usage, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getTaxonomicUsageFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("taxonUsageRating","tsn")
    itis_parse(toget, out, namespaces)
  }
}

#' Get tsn by vernacular language
#'
#' @param language A string containing the language. This is a language string,
#'    not the international language code (character)
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' gettsnbyvernacularlanguage("french", "api")
#' gettsnbyvernacularlanguage("french", "local")
#' }
#' @export
#' @keywords internal
gettsnbyvernacularlanguage <- function(language, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("select",
        paste("CASE", paste(sapply(language, function(x) paste("WHEN language LIKE ", paste("'", x, "'", sep = ""), " THEN ", paste0("'",x,"'"), sep = ""), USE.NAMES=FALSE),collapse=" "),"END AS querystring,"),
        "tsn, vernacular_name from vernaculars where ", paste0(" language like ", sapply(language, function(x) paste("'", x, "'", sep = ""),USE.NAMES=FALSE),collapse=" OR "), " order by tsn, vernacular_name")
    temp <- itis_SQL(query)
    data.frame(language = temp$querystring, comname = temp$vernacular_name, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getTsnByVernacularLanguage", list(language = language), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    matches <- c("commonName","language","tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Gets the TSN corresponding to the LSID, or an empty result if there is no match.
#'
#' @inheritParams getfullrecordfromlsid
#' @details No local SQL method.
#' @examples \dontrun{
#' gettsnfromlsid(lsid="urn:lsid:itis.gov:itis_tsn:28726", config=timeout(3))
#' gettsnfromlsid("urn:lsid:itis.gov:itis_tsn:0", config=timeout(3))
#' }
#' @export
#' @keywords internal
gettsnfromlsid <- function(lsid, ...) {
	out <- itis_GET("getTSNFromLSID", list(lsid = lsid), ...)
  if( !is.na( suppressWarnings(as.numeric(xmlToList(out)[[1]])) ) )
    { suppressWarnings( as.numeric(xmlToList(out)[[1]]) )} else
      {"invalid TSN"}
}

#' Returns the unacceptability reason, if any, for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getunacceptabilityreasonfromtsn(183671, "api")
#' getunacceptabilityreasonfromtsn(183671, "local")
#' }
#' @export
#' @keywords internal
getunacceptabilityreasonfromtsn <- function(tsn, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT tsn, unaccept_reason FROM taxonomic_units WHERE",
         paste(sapply(tsn, function(x) paste("tsn = ", x, sep = ""), USE.NAMES=FALSE), collapse=" OR "))
    temp <- itis_SQL(query)
    data.frame(tsn = temp$tsn, unacceptReason = temp$unaccept_reason, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getUnacceptabilityReasonFromTSN", list(tsn = tsn), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    toget <- list("tsn","unacceptReason")
    itis_parse(toget, out, namespaces)
  }
}

#' Provides a list of the unique languages used in the vernacular table.
#'
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getvernacularlanguages("api")
#' getvernacularlanguages("local")
#' }
#' @export
#' @keywords internal
getvernacularlanguages <- function(backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- "select distinct language from vernaculars order by language"
    temp <- itis_SQL(query)
    data.frame(languageNames = temp$language, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("getVernacularLanguages", list(), ...)
    namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.gov/xsd")
    nodes <- getNodeSet(out, "//ax21:languageNames", namespaces=namespaces)
    languageNames <- sapply(nodes, xmlValue)
    data.frame(languageNames = languageNames, stringsAsFactors = FALSE)
  }
}

#' Search for tsn by common name
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonname("american bullfrog", config=timeout(3))
#' searchbycommonname("ferret-badger", config=timeout(3))
#' searchbycommonname("polar bear", "api")
#'
#' searchbycommonname("american bullfrog", "local")
#' searchbycommonname("ferret-badger", "local")
#' searchbycommonname("polar bear", "local")
#' }
#' @export
#' @keywords internal
searchbycommonname <- function(x, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT",
      paste("CASE", paste(sapply(x, function(y) paste("WHEN v.vernacular_name LIKE ", paste("'%", y, "%'", sep = ""), " THEN ", paste0("'",y,"'"), sep = ""), USE.NAMES=FALSE), collapse=" "),"END AS querystring,"),
      "v.tsn as tsn, v.language as language, v.vernacular_name as commonName, t.complete_name as combinedName from vernaculars v inner join taxonomic_units t on v.tsn = t.tsn WHERE",
      paste(sapply(x, function(y) paste("v.vernacular_name like ", paste("'%", y, "%'", sep = ""), sep = ""), USE.NAMES=FALSE),collapse=" OR "), " order by querystring")
    temp <- itis_SQL(query)
    data.frame(comname = temp$commonName, sciname = temp$combinedName, lang = temp$language, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("searchByCommonName", list(srchKey = x), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
    comname <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
    lang <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
    tsn <- sapply(nodes, xmlValue)
    data.frame(comname=comname, lang=lang, tsn=tsn[-1], stringsAsFactors = FALSE)
  }
}

#' Searches common name and acts as thin wrapper around \code{searchbycommonnamebeginswith} and \code{searchbycommonnameendswith}
#'
#' @param x Search terms
#' @param from Default is to search from beginning. Use \code{end} to serch from end.
#' @param backend Defaults to NULL, deferring to options set by \code{\link{backend_set}}.
#' Alternatively, you can pass in one of api or local, which will only override the
#' current function call.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @seealso searchbycommonnamebeginswith searchbycommonnameendswith
#' @return \code{data.frame}
#' @examples \dontrun{
#' itis_searchcommon("inch", backend="api")
#' itis_searchcommon("inch", backend="local")
#' itis_searchcommon("inch", from = "end", "api")
#' itis_searchcommon("inch", from = "end", "local")
#'}
itis_searchcommon <- function(x, from = "begin", backend = NULL, ...) {
  switch(from,
         begin = searchbycommonnamebeginswith(x = x, backend = backend, ...),
         end = searchbycommonnameendswith(x = x, backend = backend, ...),
  )
}

#' Search for tsn by common name beginning with
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonnamebeginswith("inch", "api")
#' searchbycommonnamebeginswith("inch", "local")
#' }
#' @export
#' @keywords internal
searchbycommonnamebeginswith <- function(x, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT",
        paste("CASE", paste(sapply(x, function(y) paste("WHEN v.vernacular_name LIKE ", paste("'%", y, "%'", sep = ""), " THEN ", paste0("'", y,"'"), sep = ""), USE.NAMES=FALSE),collapse=" "),"END AS querystring,"),
        "v.tsn as tsn, v.language as language, v.vernacular_name as commonName, t.complete_name as combinedName
        from vernaculars v
        inner join taxonomic_units t on v.tsn = t.tsn WHERE",
        paste(sapply(x, function(y) paste("v.vernacular_name like ", paste("'", y, "%'", sep = ""), sep = ""), USE.NAMES=FALSE), collapse=" OR "), " order by querystring")
    temp <- itis_SQL(query)
    data.frame(comname = temp$commonName, sciname = temp$combinedName, lang = temp$language, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("searchByCommonNameBeginsWith", list(srchKey = x), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
    comname <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
    lang <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
    tsn <- sapply(nodes, xmlValue) # last one is a repeat
    nodes <- getNodeSet(out, "//ax21:sciName", namespaces=namespaces)
    data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)], stringsAsFactors = FALSE)
  }
}

#' Search for tsn by common name ending with
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonnameendswith("snake", "api")
#' searchbycommonnameendswith("snake", "local")
#' }
#' @export
#' @keywords internal
searchbycommonnameendswith <- function(x, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT",
          paste("CASE", paste(sapply(x, function(y) paste("WHEN v.vernacular_name LIKE ", paste("'%", y, "%'", sep = ""), " THEN ", paste0("'",y,"'"), sep = ""), USE.NAMES=FALSE),collapse=" "),"END AS querystring,"),
          "v.tsn as tsn, v.language as language, v.vernacular_name as commonName, t.complete_name as combinedName
      from vernaculars v
      inner join taxonomic_units t on v.tsn = t.tsn WHERE",
      paste(sapply(x, function(y) paste("v.vernacular_name like ", paste("'%", y, "'", sep = ""), sep = ""), USE.NAMES=FALSE),collapse=" OR "), " order by querystring")

    temp <- itis_SQL(query)
    data.frame(comname = temp$commonName, sciname = temp$combinedName, lang = temp$language, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("searchByCommonNameEndsWith", list(srchKey = x), ...)
    namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
    nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
    comname <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
    lang <- sapply(nodes, xmlValue)
    nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
    tsn <- sapply(nodes, xmlValue) # last one is a repeat
    data.frame(comname=comname, lang=lang, tsn=tsn[!nchar(tsn) == 0], stringsAsFactors = FALSE)
  }
}

#' Search by scientific name
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbyscientificname("Tardigrada", "api")
#' searchbyscientificname("Tardigrada", "local")
#' searchbyscientificname("Quercus douglasii", "api")
#' searchbyscientificname("Quercus douglasii", "local")
#' }
#' @export
#' @keywords internal
searchbyscientificname <- function(x, backend = NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- paste("SELECT",
      paste("CASE", paste(sapply(x, function(y) paste("WHEN t.complete_name LIKE ", paste("'%", y, "%'", sep = ""), " THEN ", paste0("'",y,"'"), sep = ""), USE.NAMES=FALSE),collapse=" "),"END AS querystring,"),
      "t.tsn as tsn, t.complete_name as combinedName
      FROM taxonomic_units t
      WHERE",
      paste(sapply(x, function(y) paste("t.complete_name like ", paste("'%", y, "%'", sep = ""), sep = ""), USE.NAMES=FALSE),collapse=" OR "), " order by querystring")
    temp <- itis_SQL(query)
    data.frame(combinedname = temp$combinedName, tsn = temp$tsn, stringsAsFactors = FALSE)
  } else {
    out <- itis_GET("searchByScientificName", list(srchKey = x), ...)
    namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
    matches <- c("combinedName","tsn")
    itisdf(out, namespaces, matches, tolower(matches))
  }
}

#' Search for any match
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchforanymatch(202385, "api")
#' searchforanymatch(202385, "local")
#' searchforanymatch("dolphin", "api")
#' searchforanymatch("dolphin", "local")
#' }
#' @export
#' @keywords internal
searchforanymatch <- function(x, backend=NULL, ...) {
  backend <- get_back(backend)
  if( backend == "local" ) {
    query <- get_query(x)
    temp <- itis_SQL(query)
    get_df(x, temp)
  } else {
    out <- itis_GET("searchForAnyMatch", list(srchKey = x), ...)
    namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
    if(is.character(x)){
      me <- getNodeSet(out, "//ax21:anyMatchList", namespaces=namespaces)
      comname <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["commonName"]]))
      comname_lang <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["language"]]))
      sciname <- sapply(me, function(x) xmlValue(x[["sciName"]]))
      tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
      data.frame(tsn=tsn, sciname=sciname, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
    } else
    {
      me <- getNodeSet(out, "//ax21:commonNames", namespaces=namespaces)
      comname <- sapply(me, function(x) xmlValue(x[["commonName"]]))
      comname_tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
      comname_lang <- sapply(me, function(x) xmlValue(x[["language"]]))
      data.frame(tsn=comname_tsn, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
    }
  }
}

get_query <- function(x){
  if(is.numeric(x)){
    paste("SELECT",
        paste("CASE", paste(sapply(x, function(y) paste("WHEN t.tsn = ", y, " THEN ", y, sep = ""), USE.NAMES=FALSE), collapse=" "),"END AS querystring,"),
        "t.tsn as tsn, t.complete_name as combinedName,
      null as commonName, null as language, 'TSN' as  matchType
      from taxonomic_units t
      WHERE", paste(sapply(x, function(y) paste("t.tsn = ", y, sep = ""), USE.NAMES=FALSE), collapse=" OR "), " order by querystring")
  } else {
    paste("SELECT",
      paste("CASE", paste(sapply(x, function(y) paste("WHEN v.vernacular_name like ", paste("'%", y, "%'", sep = ""), " THEN ", paste0("'",y,"'"), sep = ""), USE.NAMES=FALSE), collapse=" "),"END AS querystring,"),
      "t.tsn as tsn, t.complete_name as combinedName, v.vernacular_name as commonName, v.language as language,
      k.kingdom_name as kingdom
      from taxonomic_units t
      join kingdoms k on t.kingdom_id = k.kingdom_id
      inner join vernaculars v
      on v.tsn = t.tsn WHERE",
      paste(sapply(x, function(y) paste("v.vernacular_name like ", paste("'%", y, "%'", sep = ""), sep = ""), USE.NAMES=FALSE), collapse=" OR "), " order by querystring")
  }
}

get_df <- function(x, temp){
  if(is.numeric(x))
    data.frame(combinedname = temp$combinedName, tsn = temp$tsn, stringsAsFactors = FALSE)
  else
    data.frame(tsn = temp$tsn, combinedName = temp$combinedName, commonName = temp$commonName, stringsAsFactors = FALSE)
}

#' Search for any matched page
#'
#' @inheritParams getanymatchcount
#' @param pagesize An integer containing the page size (numeric)
#' @param pagenum An integer containing the page number (numeric)
#' @param ascend A boolean containing true for ascending sort order or false
#'    for descending (logical)
#' @details No local SQL method.
#' @examples \dontrun{
#' searchforanymatchpaged(202385, pagesize=100, pagenum=1, ascend=FALSE, config=timeout(3))
#' searchforanymatchpaged("Zy", pagesize=100, pagenum=1, ascend=FALSE, config=timeout(3))
#' }
#' @export
#' @keywords internal
searchforanymatchpaged <- function(x, pagesize = NULL, pagenum = NULL, ascend = NULL, ...) {
  args <- taxize_compact(list(srchKey=x, pageSize=pagesize, pageNum=pagenum, ascend=ascend))
	out <- itis_GET("searchForAnyMatchPaged", args, ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))

	if(is.character(x)){
	  me <- getNodeSet(out, "//ax21:anyMatchList", namespaces = namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["commonName"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["language"]]))
	  sciname <- sapply(me, function(x) xmlValue(x[["sciName"]]))
	  tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  data.frame(tsn=tsn, sciname=sciname, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	} else
	{
	  me <- getNodeSet(out, "//ax21:commonNames", namespaces = namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonName"]]))
	  comname_tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["language"]]))
	  data.frame(tsn=comname_tsn, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	}
}
