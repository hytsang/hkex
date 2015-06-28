library(rvest)
library(data.table)
library(stringr)

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

gettable <- function(corpnumber, baseurl = "http://sdinotice.hkex.com.hk/di/") {
    print(corpnumber)
    s <- html_session(paste0("http://sdinotice.hkex.com.hk/di/NSSrchCorpList.aspx?sa1=cl&scsd=28/06/2014&sced=28/06/2015&sc=", corpnumber, "&src=MAIN&lang=EN"))
    print(s); print("start")
    namespage <- html(s)
    namespageallnoticeslinks <- html_attr(html_nodes(namespage, "a:nth-child(11)"), "href")
    allnoticestable <- data.table()
    for (url in namespageallnoticeslinks) {
        snotice <- jump_to(s, url)
        print(snotice); print("notices")
        allnoticestablepage <- html(snotice)
        allnoticestable1 <- data.table(html_table(html_node(allnoticestablepage, "#grdPaging"), header = TRUE))
        allnoticestable1 <- allnoticestable1[`Reason for disclosure` != " "]

        allnoticestable1links <- html_attr(html_nodes(allnoticestablepage, ".tbCell:nth-child(7) a"), "href")
        getlinkinfo <- function(linkurl, baseurl = "http://sdinotice.hkex.com.hk/di/") {
            print(linkurl)
            slink <- jump_to(snotice, linkurl)
            linkpage <- html(slink)
            
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
        }

        linkinfolist <- lapply(allnoticestable1links, getlinkinfo)
        linkinfotable <- rbindlist(linkinfolist, fill = TRUE)
        
        allnoticestable1 <- cbind(allnoticestable1, linkinfotable)
        
        allnoticestable1[,corpnumber := corpnumber]

        if (nrow(allnoticestable1) > 0) {
            allnoticestable <- rbindlist(list(allnoticestable, allnoticestable1), fill=TRUE)
        }
    }
    return(allnoticestable)
}

allnoticeslist <- lapply(allstockstable[,`STOCK CODE`][1:2], gettable)
allallnoticestable <- rbindlist(allnoticeslist, fill=TRUE)

