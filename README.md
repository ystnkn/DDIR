# DDIR
R package to utilize DDI as personal tools for social research data analysis


## USAGE

```
#### sample code to use 'DDIR'
require(DDIR)

### specifying URLs
ddi.file.url  <- system.file("extdata","samplesurvey2018_ddi.xml",package="DDIR")
data.file.url <- system.file("extdata","samplesurvey2018_data.csv",package="DDIR")

### take a look on contents of files
xml2::read_xml(ddi.file.url)
readr::read_csv(data.file.url)

### example of read.codebook()
#### combining a DDI metadata and a csv data to generate a tibble object
read.codebook(ddi.file.url, data.file.url) -> dataset
head(dataset)

### checking contents of dataframe
head(sample.df)
names(sample.df)

## checking attributes
attributes(sample.df)

## function show.question()
sample.df %>% show.question(Q1)
sample.df %>% show.question(Q2)
sample.df %>% show.question(Q3)

## function show.varlabel()
sample.df %>% show.varlabel(Q1)
sample.df %>% show.varlabel(Q2)
sample.df %>% show.varlabel(Q3)

## function show.valuelabel()
sample.df %>% show.valuelabel(Q1)
sample.df %>% show.valuelabel(Q2)
sample.df %>% show.valuelabel(Q3)



#### example of mkDDI.metadata()
### specifying URL
meta.file.url <- system.file("extdata","samplesurvey2020_meta.csv",package="DDIR")
### importing a csv file which contains meta informations
meta <- readr::read_csv(meta.file.url)
##### format of the csv file:
## col1 "title"
## col2 "data.file.name"
## col3 "ddi.file.name"
## col4 "var.ID"
## col5 "var.name"
## col6 "var.qstnLit"
## col7 "var.catgry.catValu"
## col8 "var.catgry.labl"   

### take a look on contents of files
head(meta)
names(meta)

## generating a DDI xml object
mkDDI.metadata(title              = unlist(meta[,1]%>%na.omit()),
               data.file.name     = unlist(meta[,2]%>%na.omit()),
               var.ID             = unlist(meta[,4]),
               var.name           = unlist(meta[,5]),
               var.qstnLit        = unlist(meta[,6]),
               var.catgry.catValu = unlist(meta[,7]),
               var.catgry.labl    = unlist(meta[,8])
               ) 
```