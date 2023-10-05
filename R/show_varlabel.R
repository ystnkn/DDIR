#' Show label information of a variable
#'
#' @param data a data frame object created with read.codebook()
#' @param var.name single text object of a varialbe name (could be non-quoted)
#'
#' @return This function returns a label of a variable, which are retrieved from '//dataDscr//var@name' in a ddi meta data file. 
#'
#' @examples
#' ddi.file.url  <- system.file("extdata","samplesurvey2018_ddi.xml",package="DDIR")
#' data.file.url <- system.file("extdata","samplesurvey2018_data.csv",package="DDIR")
#'
#' read.codebook(ddi.file.url, data.file.url) -> dataset
#' 
#' show_question(dataset,'Q2')
#' show_question(dataset$Q2)
#'
#' require(tidyverse)
#' dataset %>% show_varlabel(Q2)
#' dataset %>% show_varlabel('Q2')
#' 
#' @export
show_varlabel <- function(data=data, var.name=var.name){
  ifelse(missing(var.name),
         attr(data,'varlabel'),
         data[[quo_name(enquo(var.name))]] %>% attr('varlabel')
         )
}

