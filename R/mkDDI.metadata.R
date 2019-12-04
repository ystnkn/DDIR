#' Make a DDI meta data file from reserch informations
#' @import tidyverse
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tibble
#' @param title single text object of the research title
#' @param data.file.name single text object of a data file name (including path information)
#' @param ddi.file.name single text object of a ddi file name (including path information)
#' @param var.ID list of variable IDs
#' @param var.name list of variable names
#' @param var.qstnLit list of questions
#' @param var.catgry.catValu list of categorical values (NA for non-categorical variables)
#' @param var.catgry.labl list of labels for categorical values (NA for non-categorical variables)
#' @return This function returns a DDI xml object 
#'
#' @examples
#'
#' meta.file.url <- system.file("extdata","samplesurvey2020_meta.csv",package="DDIR")
#' meta <- read_csv(meta.file.url)
#' 
#' mkDDI.metadata(title              = unlist(meta[,1]%>%na.omit()),
#'                data.file.name     = unlist(meta[,2]%>%na.omit()),
#'                var.ID             = unlist(meta[,4]),
#'                var.name           = unlist(meta[,5]),
#'                var.qstnLit        = unlist(meta[,6]),
#'                var.catgry.catValu = unlist(meta[,7]),
#'                var.catgry.labl    = unlist(meta[,8])
#'                ) 
#' 
#' @export

mkDDI.metadata <- function(title,data.file.name,ddi.file.name,var.ID,var.name,var.qstnLit,var.catgry.catValu,var.catgry.labl){
    ## 'mk.docDscr'
    docDscr.xml <- tibble(title=title) %>%  DDIR::mk.docDscr()
    ## 'mk.stdyDscr'
    stdyDscr.xml <- tibble(title=title) %>% DDIR::mk.stdyDscr()
    ## 'mk.fileDscr'
    fileDscr.xml <- tibble(data.file.name=data.file.name) %>% DDIR::mk.fileDscr()
    ## 'mk.dataDscr'
    dataDscr.xml <- DDIR::mk.dataDscr(var.ID,
                                var.name,
                                var.qstnLit,
                                var.catgry.catValu,
                                var.catgry.labl
                                )
    ## generating a new xml object 'codeBook'
    codeBook <- xml2::xml_new_root("codeBook",
                                   version="2.5" ,
                                   ID=""
                                   )
    
    ## combining all informations into one 'codeBook' xml object
    codeBook %>%
        xml2::xml_add_child(docDscr.xml) %>%
        xml2::xml_add_sibling(stdyDscr.xml) %>%
        xml2::xml_add_sibling(fileDscr.xml) %>%
        xml2::xml_add_sibling(dataDscr.xml)

    return(codeBook)
}
