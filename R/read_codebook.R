#' 
#'
#' Import meta data from a ddi xml file, and combine its informations with raw csv data file as a dataframe
#' @importFrom stats setNames
#' @import tidyverse
#' @import xml2
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import readr
#' @importt tibble
#' @param ddi.file.url single text object(URI of a DDI2.5 metadata xml file)
#' @param data.file.url single text object(URI of a data csv file)
#'
#' @return This function returns a data frame object with some meta data informations. If a variable contains 'catgry' information in its metadata, the variable is coded as a factor. The csv file could be without header informations. 'ID' of 'var' is used as variable names in the data frame. 'name' of 'var' and 'qstnlit' of 'qstn' are respectively attached as attributes variables. These attributes can be extracted by a general function for attributes(e.g. attributes()), or functions of this package(show.question(), show.varlabel(), show.valuelabel()).
#'
#' @examples
#'
#' ddi.file.url  <- system.file("extdata","samplesurvey2018_ddi.xml",package="DDIR")
#' data.file.url <- system.file("extdata","samplesurvey2018_data.csv",package="DDIR")
#'
#' read_codebook(ddi.file.url, data.file.url) -> dataset
#' head(dataset)
#' attributes(dataset)
#' attributes(dataset$Q1)
#' attributes(dataset$Q2)
#' 
#' @export

read_codebook <- function(ddi.file.url,data.file.url){
readr::read_csv(data.file.url, col_names=FALSE) %>%  
  ## setting variable names
  stats::setNames(xml2::read_xml(ddi.file.url) %>%
           xml2::xml_ns_strip() %>%
           xml2::xml_find_all("//dataDscr//var") %>%
           xml2::xml_attr(.,'ID')) %>%
  ## getting category information
  map2_dfc(.,(
    xml2::read_xml(ddi.file.url) %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all("//dataDscr//var")%>%
    map(~{      
      labl <- ifelse(xml2::xml_find_lgl(., "boolean(.//catgry)"),
      (xml2::xml_find_all(., ".//catgry//labl//text()") %>%
       xml2::as_list() %>%
       simplify() %>%
       tibble::as_tibble() %>%
       mutate(value=as.character(value)) %>%
       select(value)),
      NA) %>% setNames('labl')
      catValu <- ifelse(xml2::xml_find_lgl(., "boolean(.//catgry)"),
      (xml2::xml_find_all(., ".//catgry//catValu//text()") %>%
       xml2::as_list() %>%
       simplify() %>%
       tibble::as_tibble() %>%
       mutate(value=as.character(value)) %>%
       select(value)),
      NA) %>% setNames('catValu')
      cbind.data.frame(catValu,labl) %>% set_tidy_names() %>% as_tibble()
    })),
    ~{structure(.x,catgry=.y)}
    ) %>%
  ## converting categorical variable as factor
  modify_if(.,unlist(map(.,~(!anyNA(attr(.,'catgry'))))),~{
    attributes(.) -> atts
    factor(.,labels=attr(.,'catgry')$labl,levels=attr(.,'catgry')$catValu)
  }) %>% 
  ## setting category information as attributes
  map2_dfc(.,(
    xml2::read_xml(ddi.file.url) %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all("//dataDscr//var")%>%
    map(~{      
      labl <- ifelse(xml2::xml_find_lgl(., "boolean(.//catgry)"),
      (xml2::xml_find_all(., ".//catgry//labl//text()") %>%
       xml2::as_list() %>%
       simplify() %>%
       tibble::as_tibble() %>%
       mutate(value=as.character(value)) %>%
       select(value)),
      NA) %>% setNames('labl')
      catValu <- ifelse(xml2::xml_find_lgl(., "boolean(.//catgry)"),
      (xml2::xml_find_all(., ".//catgry//catValu//text()") %>%
       xml2::as_list() %>%
       simplify() %>%
       tibble::as_tibble() %>%
       mutate(value=as.character(value)) %>%
       select(value)),
      NA) %>% setNames('catValu')
      cbind.data.frame(catValu,labl) %>% set_tidy_names() %>% as_tibble()
    })),
    ~{structure(.x,catgry=.y)}
    ) %>% 
  ## setting var name information as attributes
  map2_dfc(.,(
    xml2::read_xml(ddi.file.url) %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all("//dataDscr//var") %>%
    xml2::xml_attr(.,'name'))
   ,~{structure(.x,varlabel=.y)}    
    ) %>%
  ## setting var qstn information as attributes
  map2_dfc(.,(
    xml2::read_xml(ddi.file.url) %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all("//dataDscr//var//qstn//text()") %>%
    xml2::as_list()%>%
    unlist())
 ,~{structure(.x,qstn=.y)}
   ) %>%
  structure(
    title = xml2::read_xml(ddi.file.url) %>%
      xml2::xml_ns_strip() %>%
      xml2::xml_find_all("//stdyDscr//citation//titlStmt//text()") %>%
      xml2::as_list()%>%
      unlist()
   )

}

