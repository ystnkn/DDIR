#' Make a DDI meta data file from reserch informations
#' @import tidyverse
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tibble
#' @param filename single text object of a ddi file name (including path information)
#' @param docDscr a xml object of docDscr
#' @param stdyDscr a xml object of stdyDscr
#' @param fileDscr a xml object of fileDscr
#' @param dataDscr a xml object of dataDscr
#' @return This function returns a DDI xml object 
#'
#' @examples
#'
#' meta.file.url <- system.file("extdata","samplesurvey2020_meta.csv",package="DDIR")
#' meta <- read_csv(meta.file.url)
#' 
#' ## example to use 'write.codebook'
#' write_codebook(filename = 'codebook.xml',
#'                docDscr  = docDscr.xml,
#'                stdyDscr = stdyDscr.xml,
#'                fileDscr = fileDscr.xml,
#'                dataDscr = dataDscr.xml
#'                )
#' 
#' @export


write_codebook <-  function(filename = 'codebook.xml',
                            docDscr  = docDscr.xml,
                            stdyDscr = stdyDscr.xml,
                            fileDscr = fileDscr.xml,
                            dataDscr = dataDscr.xml
                            ){
    ## generating a new xml object 'codeBook'
    codeBook <- xml2::xml_new_root("codeBook",
                   version="2.5" ,
                   ID=""
                   )

    ## combining all informations into one 'codeBook' xml object
    codeBook %>%
        xml2::xml_add_child(docDscr) %>%
        xml2::xml_add_sibling(stdyDscr) %>%
        xml2::xml_add_sibling(fileDscr) %>%
        xml2::xml_add_sibling(dataDscr)

    ## writing 'codeBook' into a file
    xml2::write_xml(codeBook,file=filename)
}



