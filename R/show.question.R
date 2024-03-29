#' Show question sentenses of a variable
#'
#' @param data a data frame object created with read.codebook()
#' @param var.name single text object of a varialbe name (could be non-quoted)
#'
#' @return This function returns a question sentense of the varialbe, which is retrieved from '//dataDscr//var//qstn//text()' in a ddi meta data file.
#'
#' @examples
#' ddi.file.url  <- system.file("extdata","samplesurvey2018_ddi.xml",package="DDIR")
#' data.file.url <- system.file("extdata","samplesurvey2018_data.csv",package="DDIR")
#'
#' read.codebook(ddi.file.url, data.file.url) -> dataset
#'
#' show.question(dataset,'Q1')
#' show.question(dataset$Q1)
#'
#' require(tidyverse)
#' dataset %>% show.question(Q1)
#' dataset %>% show.question('Q1')
#' 
#' @export

show.question <- function(data=data, var.name=var.name){
  ifelse(missing(var.name),
         attr(data,'qstn'),
         data[[quo_name(enquo(var.name))]] %>% attr('qstn')
         )
}
