#' Make a DDI meta data file from reserch informations
#' @importFrom XML newXMLDoc
#' @importFrom XML newXMLNode
#' @importFrom XML saveXML
#' @param title single text object of the research title
#' @param data.file.name single text object of a data file name (including path information)
#' @param ddi.file.name single text object of a ddi file name (including path information)
#' @param var.ID list of variable IDs
#' @param var.name list of variable names
#' @param var.qstnLit list of questions
#' @param var.catgry.catValu list of categorical values (NA for non-categorical variables)
#' @param var.catgry.labl list of labels for categorical values (NA for non-categorical variables)
#' @export
mkDDI.metadata <- function(title,data.file.name,ddi.file.name,var.ID,var.name,var.qstnLit,var.catgry.catValu,var.catgry.labl){
##require(XML)
ddi <- newXMLDoc()
codeBook <- XML::newXMLNode("codeBook",
                   attrs = c(version="2.5" ,ID=""), 
                   doc=ddi)
docDscr <- XML::newXMLNode("docDscr",parent=codeBook)
stdyDscr <- XML::newXMLNode("stdyDscr",parent=codeBook)
fileDscr <- XML::newXMLNode("fileDscr",parent=codeBook)
dataDscr <- XML::newXMLNode("dataDscr",parent=codeBook)
### docDscr
docDscr.citation <- XML::newXMLNode("citation",parent=docDscr)
docDscr..titlStmt <- XML::newXMLNode("titlStmt",paste('Metadata record for',title),parent=docDscr.citation)
docDscr..prodStmt <- XML::newXMLNode("prodStmt",parent=docDscr.citation)
docDscr..prodStmt.prodDate <- XML::newXMLNode("prodDate",paste(date()),parent=docDscr..prodStmt)
docDscr..prodStmt.software <- XML::newXMLNode("software",paste('DDIR'),parent=docDscr..prodStmt)
docDscr..verStmt <- XML::newXMLNode("verStmt",paste(date()),parent=docDscr.citation)
### stdyDscr
stdyDscr.citation <- XML::newXMLNode("citation",parent=stdyDscr)
stdyDscr..titlStmt <- XML::newXMLNode("titlStmt",paste(title),parent=stdyDscr.citation)
### fileDscr
fileDscr.citation <- XML::newXMLNode("citation",parent=fileDscr)
fileDscr..fileTxt <- XML::newXMLNode("fileTxt",parent=fileDscr.citation)
fileDscr..fileTxt.fileName <- XML::newXMLNode("fileName",paste(data.file.name),parent=fileDscr..fileTxt)
fileDscr..fileTxt.dimensns <- XML::newXMLNode("dimensns",parent=fileDscr..fileTxt)
fileDscr..fileTxt.dimensns.caseQnty <- XML::newXMLNode("caseQnty",parent=fileDscr..fileTxt.dimensns)
fileDscr..fileTxt.dimensns.varQnty <- XML::newXMLNode("varQnty",parent=fileDscr..fileTxt.dimensns)
fileDscr..fileTxt.fileType <- XML::newXMLNode("fileType",'ASCII',attrs=c(charset='UTF-8'),parent=fileDscr..fileTxt)
### dataDscr
##dataDscr.var <- newXMLNode("var",parent=dataDscr,
##                           attrs=c(
##                               ID=paste(var.ID[1]),
##                               name=paste(var.name[1])))
##dataDscr.var.qstn <- newXMLNode("qstn",parent=dataDscr.var)
##dataDscr.var.qstn.qstnLit <- newXMLNode("qstnLit",
##                                        paste(var.qstnLit[1]),
##                                        parent=dataDscr.var.qstn)
var.func <- function(ID=var.ID,name=var.name,qstnLit=var.qstnLit,catValu=var.catgry.catValu,labl=var.catgry.labl){
dataDscr.var <- XML::newXMLNode("var",parent=dataDscr,
                           attrs=c(
                               ID=paste(ID),
                               name=paste(name)))
dataDscr.var.qstn <- XML::newXMLNode("qstn",parent=dataDscr.var)
dataDscr.var.qstn.qstnLit <- XML::newXMLNode("qstnLit",
                                        paste(qstnLit),
                                        parent=dataDscr.var.qstn)
##if(!is.na(catValu)){
mapply(function(x,y){
    if(!is.na(x[1])){## x=catValu の値がNAでなければラベルをつける処理, [1]をつけないと条件の長さの警告が出る
        dataDscr.var.catgry <- XML::newXMLNode("catgry",parent=dataDscr.var)
        dataDscr.var.catgry.catValu <- XML::newXMLNode("catValu",
                                                  paste(x),
                                                  parent=dataDscr.var.catgry)
        dataDscr.var.catgry.labl <- XML::newXMLNode("labl",
                                               paste(y),
                                               parent=dataDscr.var.catgry)
    }
   },catValu,labl)
}
##var.func(var.ID[1],var.name[1],var.qstnLit[1])
mapply(var.func,var.ID,var.name,var.qstnLit,var.catgry.catValu,var.catgry.labl)
ddi
XML::saveXML(ddi,
             as.character(ddi.file.name),## as.character()しない、tibbleで返って来てfilepathとして不正でエラーになる
             encoding='UTF-8' ##なぜかUTF-8とすると改行がうまく出なくなる？ 
             )
}
