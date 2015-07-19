library(rvest)
library(data.table)
library(stringr)
library(httr)
library(lubridate)
library(readxl)
library(digest)

todaydate <- ymd(today())
oneyearago <- ymd(todaydate - years(1))
threemonthsago <- ymd(todaydate - months(3))
onemonthago <- ymd(todaydate - months(1))

todayprinted <- strftime(todaydate, "%Y-%m-%d")
dir.create(todayprinted)

cache <- function(url, encoding = "UTF-8") {
    urlpagename <- paste0(digest(url), collapse="")
    filepath <- paste0(todayprinted, "/", urlpagename, ".Rdata")
    if (!file.exists(filepath)) {
        urlpage <- read_html(url, encoding = encoding)
        cat(as(urlpage, "character"), file = filepath)
        return(urlpage)
    } else {
        urlpage <- read_html(filepath, encoding = "UTF-8")
        return(urlpage)
    }
}

codespage <- cache("http://sdinotice.hkex.com.hk/di/NSStdCode.htm")
codestable <- data.table(codes = html_text(html_nodes(codespage, ".txt:nth-child(1)")), descriptions = str_trim(str_replace_all(html_text(html_nodes(codespage, ".txt:nth-child(2)")), "[\r\n\t]", "")))

stockspage <- cache("https://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty.htm")
stockspagetable <- html_table(html_node(stockspage, ".table_grey_border"))
stockspagetable <- data.table(stockspagetable)
stockspagetable <- stockspagetable[,1:2,with=FALSE]
setnames(stockspagetable, unlist(stockspagetable[1]))
stockspagetable <- stockspagetable[-1]
gemstockspage <- cache("https://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdgems.htm")
gemstockspagetable <- html_table(html_node(gemstockspage, ".table_grey_border"))
gemstockspagetable <- data.table(gemstockspagetable)
gemstockspagetable <- gemstockspagetable[,1:2,with=FALSE]
gemstockspagetable <- gemstockspagetable[-1]
setnames(gemstockspagetable, colnames(stockspagetable))
allstockstable <- rbind(stockspagetable, gemstockspagetable)

getlinkinfo <- function(linkurl, s = spage, baseurl = "http://sdinotice.hkex.com.hk/di/") {
    print(linkurl)
    slink <- jump_to(s, linkurl)
    linkpage <- cache(slink)

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

gettable <- function(corpnumber, baseurl = "http://sdinotice.hkex.com.hk/di/", searchnumber = 11, firstdate = oneyearago, lastdate = todaydate) {
    print(corpnumber)
    lastdateprinted <- strftime(lastdate, "%d/%m/%Y")
    firstdateprinted <- strftime(firstdate, "%d/%m/%Y")
    firsturl <- paste0("http://sdinotice.hkex.com.hk/di/NSSrchCorpList.aspx?sa1=cl&scsd=", firstdateprinted, "&sced=", lastdateprinted, "&sc=", corpnumber, "&src=MAIN&lang=EN")
    s <- html_session(firsturl)
    print(s); print("start")
    namespage <- cache(firsturl)
    if (html_text(html_node(namespage, "#lblRecCount")) == 0) {return()}
    namespageallnoticeslinks <- html_attr(html_nodes(namespage, paste0("a:nth-child(", searchnumber, ")")), "href")
    allnoticestable <- data.table()
    company <- str_trim(tail(html_text(html_nodes(namespage, ".tbCell:nth-child(2)")), 1))
    print(company)
    for (urlnumber in 1:length(namespageallnoticeslinks)) {
#        company <- html_text(html_nodes(namespage, ".tbCell:nth-child(2)")[[urlnumber]])
        snotice <- jump_to(s, namespageallnoticeslinks[urlnumber])
        print(snotice); print("notices")
        noticestablehtml <- cache(paste0(baseurl, namespageallnoticeslinks[urlnumber]), encoding = "big5")
        pageslinks <- html_attr(html_nodes(noticestablehtml, "#lblPageIndex a"), "href")
        for (pagelink in pageslinks) {
            spage <- jump_to(snotice, pagelink)
            print(spage); print("page")
            spagehtml <- cache(spage)
            allnoticestablepage <- data.table(html_table(html_node(spagehtml, "#grdPaging"), header = TRUE))
            if (searchnumber == 5) {
                allnoticestablepage <- allnoticestablepage[`Date of relevant event` != " "]
            } else {
                allnoticestablepage <- allnoticestablepage[`Date of relevant event (dd/mm/yyyy)` != " "]
            }
            
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
    return(allnoticestable)
}

stockcodes <- allstockstable[,`STOCK CODE`]

allnoticeslist <- lapply(stockcodes, gettable, searchnumber = 11)
allallnoticestable <- rbindlist(allnoticeslist, fill=TRUE)
allallnoticestable[,numberofshares := as.numeric(str_replace_all(str_sub(`No. of shares bought / sold / involved`, 1, -4), ",", "")) ]
allallnoticestable[,pricepershare := as.numeric(str_sub(`Average price per share`, 4)) ]
allallnoticestable[,amount := numberofshares * pricepershare]
allallnoticestable[,currency := str_sub(`Average price per share`, 1, 3) ]
#setnames(allallnoticestable, "Name of allector", "name")
#allallnoticestable[,name := str_replace_all(name, fixed(","), "")]
#allallnoticestable[,name := str_replace_all(name, fixed("-"), " ")]
save(allallnoticestable, file = "allallnoticestable.Rdata")

dirnoticeslist <- lapply(stockcodes, gettable, searchnumber = 9)
alldirnoticestable <- rbindlist(dirnoticeslist, fill=TRUE)
alldirnoticestable[,numberofshares := as.numeric(str_replace_all(str_sub(`No. of shares bought / sold / involved`, 1, -4), ",", "")) ]
alldirnoticestable[,pricepershare := as.numeric(str_sub(`Average price per share`, 4)) ]
alldirnoticestable[,amount := numberofshares * pricepershare]
alldirnoticestable[,currency := str_sub(`Average price per share`, 1, 3) ]
save(alldirnoticestable, file = "alldirnoticestable.Rdata")

sharenoticeslist <- lapply(stockcodes, gettable, searchnumber = 5)
allsharenoticestable <- rbindlist(sharenoticeslist, fill=TRUE)
allsharenoticestable[,numberofshares := as.numeric(str_replace_all(str_sub(`No. of shares bought/ sold/ involved`, 1, -4), ",", "")) ]
allsharenoticestable[,pricepershare := as.numeric(str_sub(`Average price per share`, 4)) ]
allsharenoticestable[,amount := numberofshares * pricepershare]
allsharenoticestable[,currency := str_sub(`Average price per share`, 1, 3) ]
#setnames(allsharenoticestable, "Name of substantial shareholder", "name")
save(allsharenoticestable, file = "allsharenoticestable.Rdata")

write.csv(alldirnoticestable, file = "alldirnoticestable.csv", row.names = FALSE)
write.csv(allsharenoticestable, file = "allsharenoticestable.csv", row.names = FALSE)
write.csv(allallnoticestable, file = "allallnoticestable.csv", row.names = FALSE)
