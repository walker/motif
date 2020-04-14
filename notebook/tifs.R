#loading the package:
 library(xml2)
 library(rvest)
 library(stringr)
 library(jsonlite)

#view data
 #head(eats)

 ' Create Index of Available Months
 #'
 #' @description Constructs a table for finding a given table of crime data or a set of
 #'     tables (such as year to date or full year). This is largely needed for internal
 #'     use when downloading tables, but is exported for reference and troubleshooting.
 #'
 #' @usage cs_create_index()
 #'
 #' @return A tibble with all available monthly crime tables, the \code{iframe} page
 #'     they appear on, and their row number.
 #'
 #' @importFrom dplyr mutate select
 #' @importFrom httr POST warn_for_status
 #' @importFrom purrr map_df
 #' @importFrom rvest html_form html_nodes html_session html_table
 #' @importFrom tibble tibble
 #' @importFrom utils head
 #'
 #' @export
cs_create_index <- function(){

  # global bindings
  filename = value = NULL

  # url
  url <- "https://app.auditor.mo.gov/TIF/SearchTIF.aspx"

  # create session
  page <- rvest::html_session(url)

  # construct form
  form <- rvest::html_form(page)[[1]]

  # create vector of page numbers
  pages <- c(1:cs_count_pages())

  # iterate over iframe pages
  pages %>%
   purrr::map_df(~cs_get_table(url = url, session = page, form = form, page = .x)) -> out

  # construct output
  out <- dplyr::mutate(out, year = as.numeric(stringr::str_sub(date, -4)))
  out <- dplyr::mutate(out, value = paste(page, row))
  out <- dplyr::select(out, page, row, value, year)

  # return output
  return(out)
}

tif_switch_page <- function(url, session, form, page){

  # construct event argument value
  arg <- paste0("Page$",page)

  # update session
  session <- tif_request_POST(session, url, body = list(
   `__EVENTTARGET`="ctl00$ContentPlaceHolder1$gvResults",
   `__EVENTARGUMENT`=arg,
   `__VIEWSTATE`=form$fields$`__VIEWSTATE`$value,
   `__VIEWSTATEGENERATOR`=form$fields$`__VIEWSTATEGENERATOR`$value,
   `__EVENTVALIDATION`=form$fields$`__EVENTVALIDATION`$value
  ))

  # return output
  return(session)
 }

 # included since it is not exported from rvest
 tif_request_POST <- function(x, url, ...) {
   x$response <- httr::POST(url, x$config, ..., handle = x$handle)
   x$html <- new.env(parent = emptyenv(), hash = FALSE)
   x$url <- x$response$url
   x$back <- character() # can't go back after a post

   httr::warn_for_status(x$response)
   x
 }

tif_get_table <- function(url, session, form, page) {

  # global bindings
  filename = NULL

  # update session to correct page
  if (page > 1){
   session <- tif_switch_page(url = url, session = session, form = form, page = page)
  }

  # scrape table
  tbl <- rvest::html_nodes(session, "table")[[1]]
  tbl <- rvest::html_table(tbl, fill = TRUE)
  tbl <- as.data.frame(tbl)

  # create vector of file names
  vctr <- tbl$`Crime Detail`

  # remove last two entries from vector
  vctr <- utils::head(vctr, -2)

  # calculate law row position
  last <- length(vctr)+1

  # construct output
  out <- tibble::tibble(
   filename = vctr,
   row = c(2:last)
  )

  out <- dplyr::mutate(out, row = formatC(row, width = 2, format = "d", flag = "0"))
  out <- dplyr::mutate(out, page = page)
  out <- dplyr::select(out, page, row, filename)

  # return output
  return(out)

}

#' Download TIF IDs List from the Missouri Auditor's Office
#'
#' @description Downloads TIF reporting data from the Missouri Auditor's website
#'
#' @usage tif_get_data(locale)
#'
#' @param locale The locale (county) to get data for
#'
#' @return An ID list for all time.
#'
#' @export
tif_get_data <- function(locale, year, index) {

  # optionally build index
  if (missing(index) == TRUE){
    index <- tif_create_index()
  }

  # rename year
  x <- year

  # subset index
  if (missing(year)){
    index <- dplyr::filter(index, year == x)
  }

  # url
  url <- "https://app.auditor.mo.gov/TIF/SearchTIF.aspx"

  # create session
  page <- rvest::html_session(url)

  # construct form
  form <- rvest::html_form(page)[[1]]

  # download

  ## We'll Do updating the dataset later...
  # if (missing(year) == TRUE){
  #
  #   # store value
  #   value <- index$value
  #
  #   # iterate
  #   value %>%
  #     unlist(value) %>%
  #     purrr::map(~tif_download(value = .x, url = url, session = page, form = form)) -> out
  #
  #   # create list of months associated with year list object items
  #   # out %>%
  #   #   purrr::map(cs_identifyMonth) -> nameList
  #
  #   # convert list of months to vector
  #   # nameVector <- unlist(nameList, recursive = TRUE, use.names = TRUE)
  #
  #   # apply vector to data
  #   # names(out) <- nameVector
  #
  # } else if (missing(year) == FALSE){

    # store value
    value <- index$value[[1]]

    # pull table
    out <- tif_download(value = value, session = page, form = form)
  #
  # }

#Combining all the lists to form a data frame
# TIF_data <- data.frame(City=city, Project=project, Period=period, PILOTS=pilots, EATS=eats, PrincipalInterest=PI, AssessVal=AV, stringsAsFactors=FALSE)

# https://app.auditor.mo.gov/TIF/SearchTIF.aspx
# search:
# op: Search
# form_build_id: form-cOR1NdsnHPYvB-fm56BM4wrxqSdEckZsS41lS_L58SE
# form_id: mogov_gsa_search_form
# ctl00$ContentPlaceHolder1$ddlYear: #
# ctl00$ContentPlaceHolder1$ddYearEnded: #
# ctl00$ContentPlaceHolder1$ddlApprovedBy: St. Louis
# ctl00$ContentPlaceHolder1$ddlProjectName: #
# ctl00$ContentPlaceHolder1$btnSubmit: Submit

tif_download <- function(ID, session, form, TIF_data) {

  # global binding
  i_complaint = NULL

  # parse value
  page <- stringr::word(value, 1)
  row <- stringr::word(value, 2)

  # update session to correct page
  if (page > 1){
    session <- tif_switch_page(session = session, form = form, page = page)
    form <- rvest::html_form(session)[[1]]
  }

  # construct target
  target <- paste0("GridView1$ctl", row, "$lnkdownloadD")

  # generate response
  response <- tif_request_POST(session, url, body = list(
    `__EVENTTARGET`=target,
    `__EVENTARGUMENT`="",
    `__VIEWSTATE`=form$fields$`__VIEWSTATE`$value,
    `__VIEWSTATEGENERATOR`=form$fields$`__VIEWSTATEGENERATOR`$value,
    `__EVENTVALIDATION`=form$fields$`__EVENTVALIDATION`$value
  ))

  # generate output
  text <- response$response
  out <- utils::read.csv(textConnection(
    suppressMessages(httr::content(text, as = "text", encoding = "ISO-8859-1"))),
    stringsAsFactors = FALSE, na.strings=c(""," "))
  out <- dplyr::as_tibble(out)

  # convert all columns to character (to match on disk workflow)
  out <- dplyr::mutate_all(out, as.character)

  # clean-up variable names
  out <- janitor::clean_names(out)

  # search for i_complaint and correct # WHAT'S THIS ABOUT?
  # if ("i_complaint" %in% names(out) == TRUE){
  #   out <- dplyr::rename(out, complaint = i_complaint)
  # }

  # return output
  return(out)

}
