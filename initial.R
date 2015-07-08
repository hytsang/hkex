library(rvest)
library(data.table)
library(stringr)
library(httr)
library(lubridate)
library(readxl)

codespage <- html("http://sdinotice.hkex.com.hk/di/NSStdCode.htm")
codestable <- data.table(codes = html_text(html_nodes(codespage, ".txt:nth-child(1)")), descriptions = str_trim(str_replace_all(html_text(html_nodes(codespage, ".txt:nth-child(2)")), "[\r\n\t]", "")))

stockspage <- html("https://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty.htm")
stockspagetable <- html_table(html_node(stockspage, ".table_grey_border"))
stockspagetable <- data.table(stockspagetable)
stockspagetable <- stockspagetable[,1:2,with=FALSE]
setnames(stockspagetable, unlist(stockspagetable[1]))
stockspagetable <- stockspagetable[-1]
gemstockspage <- html("https://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdgems.htm")
gemstockspagetable <- html_table(html_node(gemstockspage, ".table_grey_border"))
gemstockspagetable <- data.table(gemstockspagetable)
gemstockspagetable <- gemstockspagetable[,1:2,with=FALSE]
gemstockspagetable <- gemstockspagetable[-1]
setnames(gemstockspagetable, colnames(stockspagetable))
allstockstable <- rbind(stockspagetable, gemstockspagetable)

getlinkinfo <- function(linkurl, s = spage, baseurl = "http://sdinotice.hkex.com.hk/di/") {
    print(linkurl)
    slink <- jump_to(s, linkurl)
    linkpage <- html(slink)

    if (!is.null(html_node(linkpage, "#lblDIssued"))) {
        tso <- html_text(html_node(linkpage, "#lblDIssued"))
        
        before <- html_text(html_nodes(linkpage, "#grdSh_BEvt td.txt"))
        after <- html_text(html_nodes(linkpage, "#grdSh_AEvt td.txt"))

        if(length(before) > 0) {
            
            positions <- before[seq(1,length(before),3)]
            pctbefore <- as.numeric(before[seq(3,length(before),3)])
            pctafter <- as.numeric(after[seq(3,length(before),3)])
            pctdiff <- pctafter - pctbefore
            
            linkinfotable <- data.table(tso = tso)
            for (position in 1:length(positions)) {
                linkinfotable[,positions[position] := pctdiff[position], with=FALSE]
            }
            return(linkinfotable)
        } else {
            linkinfotable <- data.table(tso = tso)
            return(linkinfotable)
        }
    } else {
        return(data.table(`Long Position` = NA))
    }
}

todaydate <- ymd(today())
threemonthsago <- ymd(todaydate - months(3))
onemonthago <- ymd(todaydate - months(1))

gettable <- function(corpnumber, baseurl = "http://sdinotice.hkex.com.hk/di/", searchnumber = 11, firstdate = threemonthsago, lastdate = todaydate) {
    print(corpnumber)
    lastdateprinted <- strftime(lastdate, "%d/%m/%Y")
    firstdateprinted <- strftime(firstdate, "%d/%m/%Y")
    firsturl <- paste0("http://sdinotice.hkex.com.hk/di/NSSrchCorpList.aspx?sa1=cl&scsd=", firstdateprinted, "&sced=", lastdateprinted, "&sc=", corpnumber, "&src=MAIN&lang=EN")
    s <- html_session(firsturl)
    print(s); print("start")
    namespage <- html(firsturl)
    namespageallnoticeslinks <- html_attr(html_nodes(namespage, paste0("a:nth-child(", searchnumber, ")")), "href")
    allnoticestable <- data.table()
    company <- str_trim(tail(html_text(html_nodes(namespage, ".tbCell:nth-child(2)")), 1))
    print(company)
    for (urlnumber in 1:length(namespageallnoticeslinks)) {
#        company <- html_text(html_nodes(namespage, ".tbCell:nth-child(2)")[[urlnumber]])
        snotice <- jump_to(s, namespageallnoticeslinks[urlnumber])
        print(snotice); print("notices")
        noticestablehtml <- html(paste0(baseurl, namespageallnoticeslinks[urlnumber]), encoding = "UTF-8")
        pageslinks <- html_attr(html_nodes(noticestablehtml, "#lblPageIndex a"), "href")
        for (pagelink in pageslinks) {
            spage <- jump_to(snotice, pagelink)
            print(spage); print("page")
            allnoticestablepage <- data.table(html_table(html_node(html(spage), "#grdPaging"), header = TRUE))
            allnoticestablepage <- allnoticestablepage[`Date of relevant event (dd/mm/yyyy)` != " "]
            
            if(nrow(allnoticestablepage)>0) {
                allnoticestablepagelinks <- html_attr(html_nodes(spage, ".tbCell:nth-child(7) a"), "href")
                linkinfolist <- lapply(allnoticestablepagelinks, getlinkinfo, s = spage)
                linkinfotable <- rbindlist(linkinfolist, fill = TRUE)
                
                allnoticestablepage <- cbind(allnoticestablepage, linkinfotable)
                
                allnoticestablepage[,corpnumber := corpnumber]
                allnoticestablepage[,company := company]


                
                if (nrow(allnoticestablepage) > 0) {
                    allnoticestable <- rbindlist(list(allnoticestable, allnoticestablepage), fill=TRUE)
                }
            }
        }
    }
    save(allnoticestable, file = paste0("notices/", corpnumber, ".Rdata"))
    return(allnoticestable)
}

stockcodes <- allstockstable[,`STOCK CODE`][1:10]


dirnoticeslist <- lapply(stockcodes, gettable, searchnumber = 9)
alldirnoticestable <- rbindlist(dirnoticeslist, fill=TRUE)
alldirnoticestable[,numberofshares := as.numeric(str_replace_all(str_sub(`No. of shares bought / sold / involved`, 1, -4), ",", "")) ]
alldirnoticestable[,pricepershare := as.numeric(str_sub(`Average price per share`, 4)) ]
alldirnoticestable[,amount := numberofshares * pricepershare]
alldirnoticestable[,currency := str_sub(`Average price per share`, 1, 3) ]
setnames(alldirnoticestable, "Name of substantial shareholder / director / chief executive", "name")
alldirnoticestable[,name := str_replace_all(name, fixed(","), "")]
alldirnoticestable[,name := str_replace_all(name, fixed("-"), " ")]
save(alldirnoticestable, file = "alldirnoticestable.Rdata")

sharenoticeslist <- lapply(stockcodes, gettable, searchnumber = 5)
allsharenoticestable <- rbindlist(sharenoticeslist, fill=TRUE)
allsharenoticestable[,numberofshares := as.numeric(str_replace_all(str_sub(`No. of shares bought / sold / involved`, 1, -4), ",", "")) ]
allsharenoticestable[,pricepershare := as.numeric(str_sub(`Average price per share`, 4)) ]
allsharenoticestable[,amount := numberofshares * pricepershare]
allsharenoticestable[,currency := str_sub(`Average price per share`, 1, 3) ]
setnames(allsharenoticestable, "Name of substantial shareholder / director / chief executive", "name")
save(allsharenoticestable, file = "allsharenoticestable.Rdata")


onemonthamountthreshold <- 10^7
threemonthamountthreshold <- 5*10^7
onemonthchangethreshold <- 0.1
threemonthchangethreshold <- 0.3

onemonthdirtable <- alldirnoticestable[currency == "HKD" & dmy(`Date of relevant event (dd/mm/yyyy)`) >= onemonthago]
threemonthdirtable <- alldirnoticestable[currency == "HKD" & dmy(`Date of relevant event (dd/mm/yyyy)`) >= threemonthsago]
onemonthsharetable <- allsharenoticestable[currency == "HKD" & dmy(`Date of relevant event (dd/mm/yyyy)`) >= onemonthago]
threemonthsharetable <- allsharenoticestable[currency == "HKD" & dmy(`Date of relevant event (dd/mm/yyyy)`) >= threemonthsago]


nettable <- function(table) {
    return(table[,list(long = sum(`Long Position`, na.rm=TRUE), short = sum(`Short Position`, na.rm=TRUE), pool = sum(`Lending Pool`, na.rm=TRUE), sumamount = sum(amount)),by=list(corpnumber, name, company)])
}
onemonthdirtablenet <- nettable(onemonthdirtable)[(long >= onemonthchangethreshold) | (short >= onemonthchangethreshold) | (pool >= onemonthchangethreshold) | (sumamount >= onemonthamountthreshold)]
threemonthdirtablenet <- nettable(threemonthdirtable)[(long >= threemonthchangethreshold) | (short >= threemonthchangethreshold) | (pool >= threemonthchangethreshold) | (sumamount >= threemonthamountthreshold)]
onemonthsharetablenet <- nettable(onemonthsharetable)[(long >= onemonthchangethreshold) | (short >= onemonthchangethreshold) | (pool >= onemonthchangethreshold) | (sumamount >= onemonthamountthreshold)]
threemonthsharetablenet <- nettable(threemonthsharetable)[(long >= threemonthchangethreshold) | (short >= threemonthchangethreshold) | (pool >= threemonthchangethreshold) | (sumamount >= threemonthamountthreshold)]

# officers
allofficers <- data.table(read_excel("Director_List.xls"))

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

allofficers[,name := capwords(`Director's English Name`, strict = TRUE)]
setnames(allofficers, "Stock Code", "corpnumber")

setkey(onemonthdirtablenet, `corpnumber`, `name`)
setkey(threemonthdirtablenet, `corpnumber`, `name`)
setkey(allofficers, `corpnumber`, `name`)
onemonthdirtablenetwithofficers <- merge(onemonthtablenet, allofficers, all.x=TRUE)
threemonthdirtablenetwithofficers <- merge(threemonthtablenet, allofficers, all.x=TRUE)

write.csv(onemonthdirtablenetwithofficers, file = "onemonthdirtablenet.csv")
write.csv(threemonthdirtablenetwithofficers, file = "threemonthdirtablenet.csv")
write.csv(onemonthsharetablenet, file = "onemonthsharetablenet.csv")
write.csv(threemonthsharetablenet, file = "threemonthsharetablenet.csv")
