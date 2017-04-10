library(dplyr)
library(tidyr)
library(jsonlite)
library(data.table)

makesmall <- function(bigjl) {
  hkexsmall <- bigjl[,c("corporation", "stock_code", "name", "surname", "othernames", "chinesename", "formtype", "formid", "date", "dateaware", "sharesbefore", "sharesafter", "relevanteventdetails")] %>% flatten %>% filter(formtype %in% c("1", "2", "3A")) %>% gather(key, value, starts_with("shares")) %>% separate(key, into = c("beforeafter", "position", "scale"), sep = "\\.") %>% filter(scale == "percent") %>% mutate(value = ifelse(is.na(value), 0, as.numeric(value))) %>% mutate(canonicalname = ifelse(!is.na(name), name, ifelse(!is.na(surname), paste0(surname, ", ", othernames), chinesename)))
  return(hkexsmall)
}

hkexdata <- list()
for (jsonfileindex in 1:length(list.files(pattern = "*.jl"))) {
  hkexdatatemp <- stream_in(file(list.files(pattern = "*.jl")[jsonfileindex]))
  hkexdata[[jsonfileindex]] <- makesmall(hkexdatatemp)
}

hkexsmall <- rbindlist(hkexdata)

write.csv(hkexsmall, file = "hkexsmalldelta.csv", row.names = FALSE)
