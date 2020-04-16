#install.packages('tidyverse')
#install.packages('xml2')
#install.packages('rvest')
#install.packages('stringr')
#install.packages('jsonlite')
#install.packages('httr')

#loading the package:
# library(xml2)
# library(rvest)
# library(stringr)
# library(jsonlite)
# library(tibble)
# library(dplyr)
# library(lubridate)
# library(httr)

tif_ids <- []

tif_get_init <- function(locale) {
  # url
  url <- "https://app.auditor.mo.gov/TIF/SearchTIF.aspx"
  
  # create session
  session <- rvest::html_session(url);
  
  # construct form
  form <- rvest::html_form(session)[[1]]
  
  filled.form <- form
  
  filled.form <- rvest::set_values(filled.form, 'ctl00$ContentPlaceHolder1$ddlApprovedBy' = locale)
  filled.form$url <- ""
  
  # Submit form
  session <- rvest::submit_form(session = session, form = filled.form)
  
  # return output
  return(session)
}

# included since it is not exported from rvest
tif_request_POST <- function(x, ...) {
  x$response <- httr::POST(x$url, x$config, body, encode = c("form"), handle = x$handle) #
  x$html <- new.env(parent = emptyenv(), hash = FALSE)
  x$url <- "https://app.auditor.mo.gov/TIF/SearchTIF.aspx"
  x$back <- character() # can't go back after a post
  
  httr::warn_for_status(x$response)
  x
}

tif_get_table <- function(session, locale) {
  # update session to correct page
  # if (page > 1){
  #  session <- tif_switch_page(url = url, session = session, form = form, page = page)
  # }

  # scrape table for Mo Auditor Database TIF IDs
  these_tif_ids <- rvest::html_nodes(session, "table tr td a") %>%
    rvest::html_attr("href") %>%
    stringr::str_match("vTIF\\.aspx\\?id\\=([0-9]{1,})+") %>%
    na.omit()
  these_tif_ids <- these_tif_ids[,-1]
  # typeof(these_tif_ids)
  tif_ids <- append(these_tif_ids, tif_ids)

  # check if next link
  chk_data <- data.frame(c("Page$Next"))
  next_link <- rvest::html_nodes(session, "table tr td a") %>%
    rvest::html_attr("href") %>%
    stringr::str_match("Page\\$Next") %>%
    na.omit() %>%
    rlang::is_empty()==FALSE
  
  if(next_link) {
    # iterate page by submitting page form
    # construct form
    session <- tif_request_POST(session, body = list(
      `__EVENTTARGET`="ctl00$ContentPlaceHolder1$gvResults",
      `__EVENTARGUMENT`="Page$Next",
      `__VIEWSTATE`=form$fields$`__VIEWSTATE`$value,
      `__VIEWSTATEGENERATOR`=form$fields$`__VIEWSTATEGENERATOR`$value,
      `__EVENTVALIDATION`=form$fields$`__EVENTVALIDATION`$value,
      `__VIEWSTATEENCRYPTED`=form$fields$`__VIEWSTATEENCRYPTED`$value,
      `search`=form$fields$`search`$value,
      `op`=form$fields$`op`$value,
      `form_build_id`=form$fields$`form_build_id`$value,
      `form_id`=form$fields$`form_id`$value,
      `ctl00$ContentPlaceHolder1$ddlYear`='#',
      `ctl00$ContentPlaceHolder1$ddYearEnded`='#',
      `ctl00$ContentPlaceHolder1$ddlApprovedBy`=locale,
      `ctl00$ContentPlaceHolder1$ddlProjectName`='#'
    ))
    return(session)
    
    # self-referential tif_get_table
    # tif_get_table(session)
  } else {
    return("Done")
  }
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
tif_get_data <- function(locale) { #locale, year
  tif_get_table(tif_get_init(locale), locale)
}


tif_get_data("St. Louis")
