#' Make a DDI meta data file from reserch informations
#' @import tidyverse
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tibble
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
#' dataDscr.xml <- mk_dataDscr(var.ID             = unlist(meta[,4]),
#'                             var.name           = unlist(meta[,5]),
#'                             var.qstnLit        = unlist(meta[,6]),
#'                             var.catgry.catValu = unlist(meta[,7]),
#'                             var.catgry.labl    = unlist(meta[,8])
#'                             ) 
#' 
#' @export

mk_dataDscr <-  function(var.ID,             
                         var.name,           
                         var.qstnLit,        
                         var.catgry.catValu,
                         var.catgry.labl){
    ### making a list ofject for dataDscr
    tibble(var.ID,
           var.name,           
           var.qstnLit) %>%
        pmap(function(var.ID,
                      var.name,           
                      var.qstnLit){
            list(var = structure(list(
                     qstn   = structure(list(qstnLit = structure(list(var.qstnLit))))
                 ),ID = var.ID, name = var.name
                 )
                 )
        }
        ) -> dataDscr

    ### converting from a list to a xml object for data Dscr
    dataDscr.xml <- xml2::as_xml_document(
                              list(dataDscr = dataDscr)
                              )

    ### adding categry informations
    tibble(var.ID,
           var.catgry.catValu,
           var.catgry.labl)  %>%
        na.omit() %>%
        pmap_df(
            function(var.ID,
                     var.catgry.catValu,
                     var.catgry.labl) {
                tibble(var.ID = var.ID,
                       catValu=unlist(str_split(var.catgry.catValu,pattern = ',')),
                       labl=unlist(str_split(var.catgry.labl,pattern = ','))
                    )
            }) %>%
        pmap(
            function(var.ID, catValu, labl){
                xml2::xml_root(dataDscr.xml) %>%
                    xml2::xml_find_first(paste('//var[@ID="', var.ID,'"]',sep='')) %>%                                        
                    xml2::xml_add_child('catgry','')  %>%
                    xml2::xml_add_child('catValu', catValu) %>%
                    xml2::xml_add_sibling('labl', labl) 
            }
        )
    return(dataDscr.xml)
}

