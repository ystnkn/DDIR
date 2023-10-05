#' Make a DDI meta data file from reserch informations
#' @import tidyverse
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tibble
#' @param title single text object of the research title
#' @return This function returns a DDI xml object 
#'
#' @examples
#'
#' meta.file.url <- system.file("extdata","samplesurvey2020_meta.csv",package="DDIR")
#' meta <- read_csv(meta.file.url)
#' 
#' stdyDscr.xml <- meta %>%
#'     select(c(1)) %>%
#'     na.omit() %>%
#'     mk_stdyDscr()
#' 
#' @export

mk_stdyDscr  <-  function(
                         title
                         ){
    ## converting a list object to a xml object
    xml2::as_xml_document(
              list(
                  stdyDscr = (
                      ### making a list ofject for stdyDscr
                      list(citation = structure(list(
                               titleStmt = list(paste('Metadata record for',title))
                           )
                           )
                           )
                      ###
                  )
              )
          )
    ##
    }
