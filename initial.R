library(rvest)
library(data.table)
library(stringr)
library(httr)
library(lubridate)

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

firstdate <- ymd("2015-07-03")
threemonthsago <- ymd(firstdate - months(3))
onemonthago <- ymd(firstdate - months(1))

gettable <- function(corpnumber, baseurl = "http://sdinotice.hkex.com.hk/di/") {
    print(corpnumber)
    if (file.exists(paste0("notices/", corpnumber, ".Rdata"))) {
        print("skip!")
        load(paste0("notices/", corpnumber, ".Rdata"))
        return(allnoticestable)
    }
    firstdateprinted <- strftime(firstdate, "%d/%m/%Y")
    threemonthsagoprinted <- strftime(threemonthsago, "%d/%m/%Y")
    firsturl <- paste0("http://sdinotice.hkex.com.hk/di/NSSrchCorpList.aspx?sa1=cl&scsd=", threemonthsagoprinted, "&sced=", firstdateprinted, "&sc=", corpnumber, "&src=MAIN&lang=EN")
    s <- html_session(firsturl)
    print(s); print("start")
    namespage <- html(firsturl)
    namespageallnoticeslinks <- html_attr(html_nodes(namespage, "a:nth-child(11)"), "href")
    allnoticestable <- data.table()
    for (url in namespageallnoticeslinks) {
        snotice <- jump_to(s, url)
        print(snotice); print("notices")
        noticestablehtml <- html(paste0(baseurl, url), encoding = "UTF-8")
        pageslinks <- html_attr(html_nodes(noticestablehtml, "#lblPageIndex a"), "href")
        for (pagelink in pageslinks) {
            spage <- jump_to(snotice, pagelink)
            print(pagelink); print("page")
            allnoticestablepage <- data.table(html_table(html_node(spage, "#grdPaging"), header = TRUE))
            allnoticestablepage <- allnoticestablepage[`Date of relevant event (dd/mm/yyyy)` != " "]
            
            if(nrow(allnoticestablepage)>0) {
                allnoticestablepagelinks <- html_attr(html_nodes(spage, ".tbCell:nth-child(7) a"), "href")
                linkinfolist <- lapply(allnoticestablepagelinks, getlinkinfo, s = spage)
                linkinfotable <- rbindlist(linkinfolist, fill = TRUE)
                
                allnoticestablepage <- cbind(allnoticestablepage, linkinfotable)
                
                allnoticestablepage[,corpnumber := corpnumber]


                
                if (nrow(allnoticestablepage) > 0) {
                    allnoticestable <- rbindlist(list(allnoticestable, allnoticestablepage), fill=TRUE)
                }
            }
        }
    }
    save(allnoticestable, file = paste0("notices/", corpnumber, ".Rdata"))
    return(allnoticestable)
}

allnoticeslist <- lapply(allstockstable[,`STOCK CODE`], gettable)
save(allnoticeslist, file = "allnoticeslist.Rdata")
allallnoticestable <- rbindlist(allnoticeslist, fill=TRUE)
allallnoticestable[,numberofshares := as.numeric(str_replace_all(str_sub(`No. of shares bought / sold / involved`, 1, -4), ",", "")) ]
allallnoticestable[,pricepershare := as.numeric(str_sub(`Average price per share`, 4)) ]
allallnoticestable[,amount := numberofshares * pricepershare]
allallnoticestable[,currency := str_sub(`Average price per share`, 1, 3) ]
save(allallnoticestable, file = "allnoticestable.Rdata")

# officers
getdirectorinfo <- function(corpnumber) {
    print(paste("officer", corpnumber))
    if (file.exists(paste0("officers/", corpnumber, "officers.Rdata"))) {print("skip!"); load(paste0("officers/", corpnumber, "officers.Rdata")); return(officertable)}
    officerpage <- html(paste0("http://www.reuters.com/finance/stocks/companyOfficers?symbol=", str_sub(corpnumber, -4, -1), ".HK&WTmodLOC=C4-Officers-5"))
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

allofficers[,c("firstname", "lastname") := tstrsplit(Name, " ", fixed=TRUE)]
allofficers[,name := paste(lastname, firstname)]

setnames(allallnoticestable, "Name of substantial shareholder / director / chief executive", "name")
setkey(allallnoticestable, `corpnumber`, `name`)
setkey(allofficers, `corpnumber`, `name`)
allallnoticestable <- merge(allallnoticestable, allofficers, all.x=TRUE)

onemonthamountthreshold <- 10^7
threemonthamountthreshold <- 5*10^7
onemonthchangethreshold <- 0.01
threemonthchangethreshold <- 0.03

onemonthtable <- allallnoticestable[dmy(`Date of relevant event (dd/mm/yyyy)`) > onemonthago]
threemonthtable <- allallnoticestable[dmy(`Date of relevant event (dd/mm/yyyy)`) > threemonthsago]
onemonthtable <- onemonthtable[(amount > onemonthamountthreshold) | (abs(`Long Position`) > onemonthchangethreshold) | (abs(`Short Position`) > onemonthchangethreshold) | (abs(`Lending Pool`) > onemonthchangethreshold)]
threemonthtable <- threemonthtable[(amount > threemonthamountthreshold) | (abs(`Long Position`) > threemonthchangethreshold) | (abs(`Short Position`) > threemonthchangethreshold) | (abs(`Lending Pool`) > threemonthchangethreshold)]

presenttable <- function(table) {
    tablepresent <- table[,list(corpnumber, name, `Reason for disclosure`, `No. of shares bought / sold / involved`, `Average price per share`, `% of issued share capital`, `Date of relevant event (dd/mm/yyyy)`, tso, `Long Position`, `Short Position`, `Lending Pool`, amount, currency, Age, `Current Position`, Since)]
    setnames(tablepresent, c("Stock code", "Name", "Disclosure code", "No. of shares", "Average price", "% interest", "Date", "TSO", "Change in long position", "Change in short position", "Change in lending pool", "Amount", "Currency", "Age", "Current Position", "Since"))
    tablepresent[,Date := dmy(Date)]
    tablepresent <- tablepresent[order(-rank(Date), rank(`Stock code`))]
    return(tablepresent)
}

onemonthtablepresent <- presenttable(onemonthtable)
threemonthtablepresent <- presenttable(threemonthtable)

write.csv(onemonthtablepresent, "onemonthtablepresent.csv")
write.csv(threemonthtablepresent, "threemonthtablepresent.csv")
