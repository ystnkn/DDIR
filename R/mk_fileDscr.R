#' Make a DDI meta data file from reserch informations
#' @import tidyverse
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tibble
#' @param data.file.name single text object of a data file name (including path information)
#' @return This function returns a DDI xml object 
#'
#' @examples
#'
#' meta.file.url <- system.file("extdata","samplesurvey2020_meta.csv",package="DDIR")
#' meta <- read_csv(meta.file.url)
#' 
#' fileDscr.xml <- meta %>%
#'     select(c(2)) %>%
#'     na.omit() %>%
#'     mk_fileDscr()
#' 
#' @export

mk_fileDscr  <-  function(
                         data.file.name
                         ){
    ## converting a list object to a xml object
    xml2::as_xml_document(
              list(
                  fileDscr = (
                      ### making a list ofject for fileDscr
                      list(citation = structure(list(
                               fileTxt = structure(list(
                                   fileName = paste(data.file.name),
                                   fileType = structure(list('ASCII'),charset = 'UTF-8'),
                                   verStmt  = structure(list(version=paste(date())))
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


