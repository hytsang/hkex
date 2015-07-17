library(shiny)
library(lubridate)
library(data.table)
library(readxl)
library(stringr)

alldirnoticestable <- fread("alldirnoticestable.csv")
allsharenoticestable <- fread("allsharenoticestable.csv")
allallnoticestable <- fread("allallnoticestable.csv")

setnames(alldirnoticestable, old = c("Name of director", "Date of relevant event (dd/mm/yyyy)"), new = c("name", "date"))
setnames(allsharenoticestable, old = c("Name of substantial shareholder", "Date of relevant event"), new = c("name", "date"))
setnames(allallnoticestable, old = c("Name of substantial shareholder / director / chief executive", "Date of relevant event (dd/mm/yyyy)"), new = c("name", "date"))

if(!file.exists("Director_List.xls")) {download.file("http://www.hkexnews.hk/reports/dirsearch/dirlist/Documents/Director_List.xls", "Director_List.xls")}
allofficers <- data.table(read_excel("Director_List.xls"))
allofficers <- allofficers[is.na(`Resignation Date (yyyy-mm-dd)`)] # only officers who haven't resigned
allofficers[,`Resignation Date (yyyy-mm-dd)` := NULL]
allofficers[,canonicalname := tolower(`Director's English Name`)]
allofficers[,canonicalname := str_replace_all(canonicalname, fixed(" "), "")]
setnames(allofficers, "Stock Code", "corpnumber")

addofficers <- function(dirtable) {

    table <- copy(dirtable)
                                        # officers
    table[,canonicalname := str_replace_all(name, fixed(","), "")]
    table[,canonicalname := str_replace_all(canonicalname, fixed("-"), " ")]
    table[,canonicalname := str_replace_all(canonicalname, fixed(" "), "")]
    table[,canonicalname := tolower(canonicalname)]

    setkey(table, `corpnumber`, `canonicalname`)
    setkey(allofficers, `corpnumber`, `canonicalname`)
    tablewithofficers <- merge(table, allofficers, all.x=TRUE)
    tablewithofficers[, c("canonicalname", "Director's English Name") := NULL]

    return(tablewithofficers)
}

nettable <- function(noticestable) {
    table <- copy(noticestable)
    if ("Short Position" %in% colnames(table)) {
        if ("Lending Pool" %in% colnames(table)) {
            return(table[,list(long = sum(`Long Position`, na.rm=TRUE), short = sum(`Short Position`, na.rm=TRUE), pool = sum(`Lending Pool`, na.rm=TRUE), sumamount = sum(amount)),by=list(corpnumber, name, company)])
        } else {
            return(table[,list(long = sum(`Long Position`, na.rm=TRUE), short = sum(`Short Position`, na.rm=TRUE), pool = 0, sumamount = sum(amount)),by=list(corpnumber, name, company)])
        }
    } else {
        if ("Lending Pool" %in% colnames(table)) {
            return(table[,list(long = sum(`Long Position`, na.rm=TRUE), short = 0, pool = sum(`Lending Pool`, na.rm=TRUE), sumamount = sum(amount)),by=list(corpnumber, name, company)])
        } else {
            return(table[,list(long = sum(`Long Position`, na.rm=TRUE), short = 0, pool = 0, sumamount = sum(amount)),by=list(corpnumber, name, company)])
        }
    }
}

noticestablelist <- list(alldirnoticestable, allsharenoticestable, allallnoticestable)

shinyServer(function(input, output) {
    noticestable <- reactive({
        noticestablelist[[as.numeric(input$whichtable)]][currency == "HKD" & dmy(date) >= ymd(as.character(input$daterange[1])) & dmy(date) <= ymd(as.character(input$daterange[2]))]
    })
    output$tablenet <- renderDataTable({
        if (input$whichtable == 1) {
            addofficers(nettable(noticestable())[(abs(long) >= input$changethreshold) | (abs(short) >= input$changethreshold) | (abs(pool) >= input$changethreshold) | (sumamount >= input$amountthreshold)])
        } else {
            nettable(noticestable())[(abs(long) >= input$changethreshold) | (abs(short) >= input$changethreshold) | (abs(pool) >= input$changethreshold) | (sumamount >= input$amountthreshold)]
        }
    })
})
