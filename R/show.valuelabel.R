#' Show label informations of categories of a variable
#'
#' @param data a data frame object created with read.codebook()
#' @param var.name single text object of a varialbe name (could be non-quoted)
#'
#' @return This function returns a data frame of categorical valus and their labels of a variable, which are retrieved from '//dataDscr//var//catgry' in a ddi meta data file. If the variable contains no 'catgry' information, this function returns NA.
#'
#' @examples
#' ddi.file.url  <- system.file("extdata","samplesurvey2018_ddi.xml",package="DDIR")
#' data.file.url <- system.file("extdata","samplesurvey2018_data.csv",package="DDIR")
#'
#' read.codebook(ddi.file.url, data.file.url) -> dataset
#' 
#' show.question(dataset,'Q2')
#' show.question(dataset$Q2)
#'
#' require(tidyverse)
#' dataset %>% show.valuelabel(Q2)
#' dataset %>% show.valuelabel('Q2')
#' 
#' @export
show.valuelabel <- function(data=data, var.name=var.name){
  ifelse(missing(var.name),
  {attr(data,'catgry') -> att
    return(data.frame(att))
  },
  {data[[quo_name(enquo(var.name))]] %>% attr('catgry') -> att
    return(data.frame(att))
  }
  )
}
