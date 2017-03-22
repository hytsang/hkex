library(dplyr)
library(tidyr)
library(jsonlite)
library(data.table)

hkexdata <- list()
for (jsonfileindex in 1:length(list.files(pattern = "*.jl"))) {
  hkexdata[[jsonfileindex]] <- stream_in(file(list.files(pattern = "*.jl")[jsonfileindex]))
}

makesmall <- function(bigjl) {
  hkexsmall <- bigjl[,c("corporation", "stock_code", "name", "surname", "othernames", "chinesename", "formtype", "formid", "date", "dateaware", "sharesbefore", "sharesafter", "relevanteventdetails")] %>% flatten %>% filter(formtype %in% c("1", "2", "3A")) %>% gather(key, value, starts_with("shares")) %>% separate(key, into = c("when", "position", "scale"), sep = "\\.") %>% filter(scale == "percent") %>% mutate(value = ifelse(is.na(value), 0, as.numeric(value))) %>% mutate(canonicalname = ifelse(!is.na(name), name, ifelse(!is.na(surname), paste0(surname, ", ", othernames), chinesename)))
  return(hkexsmall)
}

hkexsmall <- lapply(hkexdata, makesmall)
rm(hkexdata); gc()
hkexsmall <- rbindlist(hkexsmall)

write.csv(hkexsmall, file = "hkexsmall.csv", row.names = FALSE)
