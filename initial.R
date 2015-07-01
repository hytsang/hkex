library(rvest)
library(data.table)
library(stringr)
library(httr)

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
        return(data.table(`Long Position` = NA))
    }
}


gettable <- function(corpnumber, baseurl = "http://sdinotice.hkex.com.hk/di/") {
    print(corpnumber)
    if (file.exists(paste0("notices/", corpnumber, ".Rdata"))) {print("skip!"); return(c())}
    firsturl <- paste0("http://sdinotice.hkex.com.hk/di/NSSrchCorpList.aspx?sa1=cl&scsd=28/06/2014&sced=28/06/2015&sc=", corpnumber, "&src=MAIN&lang=EN")
    s <- html_session(firsturl)
    print(s); print("start")
    namespage <- html(firsturl)
    namespageallnoticeslinks <- html_attr(html_nodes(namespage, "a:nth-child(11)"), "href")
    allnoticestable <- data.table()
    for (url in namespageallnoticeslinks) {
        snotice <- jump_to(s, url)
        print(snotice); print("notices")
        allnoticestablepage <- html(paste0(baseurl, url), encoding = "UTF-8")
        pageslinks <- html_attr(html_nodes(allnoticestablepage, "#lblPageIndex a"), "href")
        for (pagelink in pageslinks) {
            spage <- jump_to(snotice, pagelink)
            allnoticestable1 <- data.table(html_table(html_node(allnoticestablepage, "#grdPaging"), header = TRUE))
            allnoticestable1 <- allnoticestable1[`Date of relevant event (dd/mm/yyyy)` != " "]
            
            if(nrow(allnoticestable1)>0) {
                allnoticestable1links <- html_attr(html_nodes(allnoticestablepage, ".tbCell:nth-child(7) a"), "href")
                linkinfolist <- lapply(allnoticestable1links, getlinkinfo, s = spage)
                linkinfotable <- rbindlist(linkinfolist, fill = TRUE)
                
                allnoticestable1 <- cbind(allnoticestable1, linkinfotable)
                
                allnoticestable1[,corpnumber := corpnumber]
                
                if (nrow(allnoticestable1) > 0) {
                    allnoticestable <- rbindlist(list(allnoticestable, allnoticestable1), fill=TRUE)
                }
            }
        }
    }
    save(allnoticestable, file = paste0("notices/", corpnumber, ".Rdata"))
#    return(allnoticestable)
}

                                        #allnoticeslist <- lapply(allstockstable[,`STOCK CODE`], gettable)

for (stock in allstockstable[,`STOCK CODE`]) {
    gettable(stock)
    gc()
}

                                        #save(allnoticeslist, file = "allnoticeslist.Rdata")

#allallnoticestable <- rbindlist(allnoticeslist, fill=TRUE)

# officers
getdirectorinfo <- function(corpnumber) {
    print(corpnumber)
    if (file.exists(paste0("officers/", corpnumber, "officers.Rdata"))) {print("skip!"); return(c())}
    corpnumber <- str_sub(corpnumber, -4, -1)
    officerpage <- html(paste0("http://www.reuters.com/finance/stocks/companyOfficers?symbol=", corpnumber, ".HK&WTmodLOC=C4-Officers-5"))
    if (!is.null(html_node(officerpage, "table.dataTable"))) {
        officertable <- data.table(html_table(html_node(officerpage, "table.dataTable")))
        officertable[,corpnumber := corpnumber]
        save(officertable, file = paste0("officers/", corpnumber, "officers.Rdata"))
        return(officertable)
    } else {
        return(data.table(corpnumber = corpnumber))
    }
}

allofficers <- rbindlist(lapply(allstockstable[,`STOCK CODE`], getdirectorinfo), fill=TRUE)
save(allofficers, file = "allofficers.Rdata")
