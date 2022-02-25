#'@title Package 'refreshr'
#'
#'@description Create refreshable dataframes/tables that automatically pull in
#'  data from an (internet) data source and transform the data (if neccessary)
#'  so that the user of your dataset does not have to worry about where to get
#'  the data from and how to update it.
#'
#'  Functions available: \itemize{ \item \code{\link{make_refreshable}()}: Makes
#'  a dataframe/table refreshable. \item \code{\link{refresh}()}: Refreshes a
#'  dataframe/table. \item \code{\link{is.refreshr}()}: Checks if a
#'  dataframe/table is set up as refreshable. \item \code{\link{uptodate}()}:
#'  Checks if a refreshable dataframe/table is up to date compared to the remote
#'  data source. \item \code{\link{properties}()}: Prints or returns the main
#'  properties of a refreshable dataframe/table. }
#'
#'@name refreshr
NULL


msg <- function(txt) {
  if(crayon::num_ansi_colors() >= 256) {
    sty <- crayon::make_style("#a610c8")
    cat(sty(txt))
  }
  else {
    cat(txt)
  }
}



check_url <- function(txt, silent = FALSE) {
  # Credits for regex to https://stackoverflow.com/users/165839/daveo [https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url]
  # Only changes: Removal of brackets and possibility of FTP addresses
  url <- stringr::str_match(txt, "[\\\"|\\\'](https?:\\/\\/(www\\.)?|ftp?:\\/\\/(ftp\\.)?)?[-a-zA-Z0-9@%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@%_\\+.~#?&\\/\\/=]*)[\\\"|\\\']")[1,1]
  url <- stringr::str_sub(url, 2, length(url)-3)
  url_formatted <- url
  cannot <- "(cannot be identified)"
  if(crayon::num_ansi_colors() >= 256) {
    sty_cannot <- crayon::make_style("#c8385c")
    sty_url <- crayon::make_style("#a610c8")
    cannot <- sty_cannot(cannot)
    url_formatted <- sty_url(url_formatted)
  }
  if(!silent) {
    ifelse(is.na(url), cannot, url_formatted)
  }
  else {
    ifelse(is.na(url), NULL, url)
  }
}



#' @title Analysing refreshr objects
#' @description Checks if a dataframe/table is refreshable.
#' @param df Dataframe/table to be checked.
#'
#' @return \code{TRUE} if the dataframe/table is of class \code{refreshr} (i.e.
#'   is of class "refreshr"), \code{FALSE} otherweise.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(dplyr)
#'
#' # Load US unemployment rate from Bureau of Labor Statistics
#' data <- fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep="\t")
#'
#' # Make refreshable and specify code for data preparation (filter raw data for
#' # the overall US employment rate) with # being a placeholder for the downloaded
#' # raw data
#' data_refresh <- make_refreshable(data,
#'                      load_code = "data.table::fread(
#'                         \"https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData\",
#'                         sep=\"\t\")",
#'                      prep_code = "filter(#, series_id==\"LNS14000000\")")
#'
#' # Save refreshable dataframe as RData file (e.g. to share dataset with coworkers or public)
#' save(data_refresh, file = "refresh.RData")
#'
#' # Remove dataframe and reload it from file
#' rm(data_refresh)
#' load(file = "refresh.RData")
#'
#' # Refresh the dataframe
#' data_refresh <- refresh(data_refresh)
#'
#' # Show properties of refreshable dataframe
#' properties(data_refresh)
#'
#' # Check if refreshable dataframe is up-to-date with the remote data source
#' uptodate(data_refresh)
#' }
is.refreshr <- function(df) {
  return("refreshr" %in% class(df))
}



#' @title Updating dataframes/tables
#' @description Checks if a refreshable dataframe/table is up-to-date with its
#'   data source.
#' @param df Dataframe/table to be checked.
#'
#' @details Please note then \code{updtodate()} needs to dowload the data from
#'   the data source and process it according to the data preparation steps
#'   defined in the \code{prep} property of the refreshable dataframe/table in
#'   order to compare it to the current data of the refreshable dataframe/table.
#'   Depending on the amount of data and the complexity of the preparation steps
#'   this may take some time.
#'
#' @return \code{TRUE} if if the dataframe/table properly reflects the state of
#'   its data source, \code{FALSE} otherweise.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(dplyr)
#'
#' # Load US unemployment rate from Bureau of Labor Statistics
#' data <- fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep="\t")
#'
#' # Make refreshable and specify code for data preparation (filter raw data for
#' # the overall US employment rate) with # being a placeholder for the downloaded
#' # raw data
#' data_refresh <- make_refreshable(data,
#'                      load_code = "data.table::fread(
#'                         \"https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData\",
#'                         sep=\"\t\")",
#'                      prep_code = "filter(#, series_id==\"LNS14000000\")")
#'
#' # Save refreshable dataframe as RData file (e.g. to share dataset with coworkers or public)
#' save(data_refresh, file = "refresh.RData")
#'
#' # Remove dataframe and reload it from file
#' rm(data_refresh)
#' load(file = "refresh.RData")
#'
#' # Refresh the dataframe
#' data_refresh <- refresh(data_refresh)
#'
#' # Show properties of refreshable dataframe
#' properties(data_refresh)
#'
#' # Check if refreshable dataframe is up-to-date with the remote data source
#' uptodate(data_refresh)
#' }
uptodate <- function(df) {
  if(is.refreshr(df)) {
    df.new <- df
    df.new <- refresh(df.new, silent = TRUE)
    return(isTRUE(dplyr::all_equal(df, df.new)))
  }
  else {
    msg(paste0("Argument ", as.character(deparse(substitute(df))), " is not a refreshable dataframe."))
  }
}



#' @title Analysing refreshr objects
#' @description Checks if a dataframe/table is refreshable.
#' @param df Dataframe/table to be checked.
#' @param property One-element Character vector describing the property thatto
#'   be queried. Either \code{"load"} for the load code (the code that refreshes
#'   data from the data source),  \code{"prep"} for the data preparation code (of
#'   any),  \code{"source"} for the data source (which \code{properties()} tries
#'   to identify from the load code),  \code{"lastrefresh"} (the date/timestamp of the last refresh of the dataframe/table). If no property is selected (\code{property == NULL}, the default) then all properties are included in the output to the screen.
#' @param silent If silent the function will return (invisibly) the property
#'   defined by \code{property} without making any outputs on the screen.
#'   Default is \code{FALSE}.
#'
#' @return if \code{property == NULL}, i.e. all properties are queried, then  \code{NULL} is returned. Otherwise \code{properties()} returns the value of the selected property.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(dplyr)
#'
#' # Load US unemployment rate from Bureau of Labor Statistics
#' data <- fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep="\t")
#'
#' # Make refreshable and specify code for data preparation (filter raw data for
#' # the overall US employment rate) with # being a placeholder for the downloaded
#' # raw data
#' data_refresh <- make_refreshable(data,
#'                      load_code = "data.table::fread(
#'                         \"https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData\",
#'                         sep=\"\t\")",
#'                      prep_code = "filter(#, series_id==\"LNS14000000\")")
#'                      #'
#' # Save refreshable dataframe as RData file (e.g. to share dataset with coworkers or public)
#' save(data_refresh, file = "refresh.RData")
#'
#' # Remove dataframe and reload it from file
#' rm(data_refresh)
#' load(file = "refresh.RData")
#'
#' # Refresh the dataframe
#' data_refresh <- refresh(data_refresh)
#'
#' # Show properties of refreshable dataframe
#' properties(data_refresh)
#'
#' # Check if refreshable dataframe is up-to-date with the remote data source
#' uptodate(data_refresh)
#' }
properties <- function(df, property = NULL, silent = FALSE) {
  if(is.refreshr(df)) {
    if(!is.null(property)) {
      if(!(property %in% c("load", "prep", "lastrefresh", "source"))) {
        stop(paste0("\"", property, "\" is not a valid property. Valid properties are \"load\", \"prep\", \"source\" and \"lastrefresh\"."))
      }
    }

    if(is.null(property)) property <- ""

    if(!silent) {
      if(property == "" | property =="lastrefresh") {
        msg(paste0("Last refresh: ", attributes(df)[["lastrefresh"]]))
        if(property == "") cat("\n")
      }

      if(property == "" | property =="source") {
        msg("Data source: ")
        cat(check_url(attributes(df)[["load"]]))
        if(property == "") cat("\n")
      }

      if(property == "") {
        msg(paste0("Structure: ", NROW(df), " ", ifelse(NROW(df)>1, "rows", "row"), " | ", NCOL(df),  " ", ifelse(NCOL(df)>1, "columns", "column")))
        cat("\n")
      }
      if(property == "" | property =="load") {
        msg("Load code: ")
        cat(crayon::blue(attributes(df)[["load"]]))
        if(property == "") cat("\n")
      }

      if(property == "" | property =="prep") {
        if(!is.null(attributes(df)[["prep"]])) {
          msg("Preparation code: ")
          cat(crayon::blue(attributes(df)[["prep"]]))
        }
      }
    }

    if(property != ""){
      if(property != "source") {
        invisible(attributes(df)[[property]])
      }
      else {
        invisible(check_url(attributes(df)[["load"]], silent = FALSE))
      }
    }
  }
  else {
    msg(paste0("Argument ", as.character(deparse(substitute(df))), " is not a refreshable dataframe."))
  }
}



#' @title Working with refreshable dataframes/tables
#' @description Refreshes a refreshable dataframes/table by downloading the data
#'   from the source and executing the data preparation code (if such code has
#'   been specified).

#' @param df The refreshed dataframe/table that is to be updated.
#' @param silent If \code{TRUE} then \code{refresh()} will not show any outputs on the screen.
#'
#' @return The refreshed dataframe/table with up-to-date data.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(dplyr)
#'
#' # Load US unemployment rate from Bureau of Labor Statistics
#' data <- fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep="\t")
#'
#' # Make refreshable and specify code for data preparation (filter raw data for
#' # the overall US employment rate) with # being a placeholder for the downloaded
#' # raw data
#' data_refresh <- make_refreshable(data,
#'                      load_code = "data.table::fread(
#'                         \"https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData\",
#'                         sep=\"\t\")",
#'                      prep_code = "filter(#, series_id==\"LNS14000000\")")
#'
#' # Save refreshable dataframe as RData file (e.g. to share dataset with coworkers or public)
#' save(data_refresh, file = "refresh.RData")
#'
#' # Remove dataframe and reload it from file
#' rm(data_refresh)
#' load(file = "refresh.RData")
#'
#' # Refresh the dataframe
#' data_refresh <- refresh(data_refresh)
#'
#' # Show properties of refreshable dataframe
#' properties(data_refresh)
#'
#' # Check if refreshable dataframe is up-to-date with the remote data source
#' uptodate(data_refresh)
#' }
refresh <- function(df, silent = FALSE) {
  if(is.refreshr(df)) {
    nobs_old <- NROW(df)
    load_code <- attributes(df)[["load"]]
    prep_code <- attributes(df)[["prep"]]
    df <- eval(parse(text = load_code), envir = sys.nframe()-1)
    if(!is.null(prep_code)) {
      prep_code_exec <- stringr::str_replace_all(prep_code, "#", "df")
      df <- eval(parse(text = prep_code_exec))
    }
    df <- make_refreshable(df, load_code, prep_code)
    attributes(df)[["lastrefresh"]] <- lubridate::now()
    nobs_new <- NROW(df)
    if(!silent) msg(paste0("Origina data set had ", nobs_old, " rows, updated dataset has ", nobs_new, "."))
    return(df)
  }
  else {
    msg(paste0("Argument ", as.character(deparse(substitute(df))), " is not a refreshable dataframe."))
    invisible(NULL)
  }
}



#' @title Making dataframes/tables refreshable
#' @description Makes a dataframe/table refreshable, i.e. connects it with a
#'   data source and specifies code that is applied to the raw data after the
#'   data has been loaded (optional).
#' @param df The dataframe/table that is to be made refreshable
#' @param load_code The code used to load the data from the data source. Please
#'   not that quotes need to be escaped (code{\"}).
#' @param prep_code The code used to transform the raw data downloaded from the
#'   data source. The placeholder \code{#} can be used in this code to refer to
#'   the data downloaded from the data source.
#'
#' @return A dataframe/table of class \code{refreshr} that can be refreshed by
#'   calling \code{refresh()}.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(dplyr)
#'
#' # Load US unemployment rate from Bureau of Labor Statistics
#' data <- fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep="\t")
#'
#' # Make refreshable and specify code for data preparation (filter raw data for
#' # the overall US employment rate) with # being a placeholder for the downloaded
#' # raw data
#' data_refresh <- make_refreshable(data,
#'                      load_code = "data.table::fread(
#'                         \"https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData\",
#'                         sep=\"\t\")",
#'                      prep_code = "filter(#, series_id==\"LNS14000000\")")
#'
#' # Save refreshable dataframe as RData file (e.g. to share dataset with coworkers or public)
#' save(data_refresh, file = "refresh.RData")
#'
#' # Remove dataframe and reload it from file
#' rm(data_refresh)
#' load(file = "refresh.RData")
#'
#' # Refresh the dataframe
#' data_refresh <- refresh(data_refresh)
#'
#' # Show properties of refreshable dataframe
#' properties(data_refresh)
#'
#' # Check if refreshable dataframe is up-to-date with the remote data source
#' uptodate(data_refresh)
#' }
make_refreshable <- function(df, load_code, prep_code = NULL) {
  attributes(df) <- append(attributes(df), list(load = load_code, lastrefresh = NA))
  if(!is.null(prep_code)) attributes(df) <- append(attributes(df), list(prep = prep_code))
  class(df) <- append("refreshr", class(df))
  return(df)
}
