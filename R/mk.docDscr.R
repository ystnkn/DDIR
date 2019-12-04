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
#' docDscr.xml <- meta %>%
#'     select(c(1)) %>%
#'     na.omit() %>%
#'     mk.docDscr()
#' 
#' @export

mk.docDscr  <-  function(
                         title
                         ){
    ## converting a list object to a xml object
    xml2::as_xml_document(
              list(
                  docDscr = (
                      ### making a list ofject for docDscr
                      list(citation = structure(list(
                               titleStmt = list(paste('Metadata record for',title)),
                               prodStmt  = structure(list(
                                   prodDate  = paste(date()),
                                   software  = paste('DDIR'),
                                   verStmt   = paste(date())
                               )
                               )
                           )
                           )
                           )
                      ### 
                  )
              )
          )
    ##
}
