library(dplyr)
library(stringr)
cat("\014")
gc(reset=T)
options(scipen=999)
options(verbose=F)
options(knitr.kable.NA = '.')
options(warn=-1)
# "lettercase",, 'formattable'
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
requiredPackages = c("stringr","rjson", "foreign","data.table", "RMySQL","RCurl","TTR", "httr",
                     "gdata", "tableHTML", "textclean", "rvest", "XML", "Rcrawler","knitr",
                     "BatchGetSymbols", "Quandl","anytime", "quantmod", "dplyr", "tibble", "base64enc",
                     "lubridate", "readxl", "outliers", "openxlsx", "countrycode",
                     "WDI", "tidyr", "vietnameseConverter", 'yfR', 'kableExtra', 'httr', 'stringi', 'stringr', 'DBI', 'RMariaDB', 'DT','jsonlite','xml2',
                     'roll')
# "RBarchart",
for (p in requiredPackages) {
  print(p, quote=F); cat("-----------------------------------------------------------------------","\n")
  if (!require(p,character.only = TRUE)){
    install.packages(p, quiet = T)
  }
  library(p,character.only = TRUE, quietly = T)
}
data_intraday = readRDS("Z:/CCPR/DATA/DASHBOARD_LIVE/DATA_INTRADAY.RDS")
dbl_source_ins_day_history = readRDS("Z:/CCPR/DATA/DASHBOARD_LIVE/DATA_DAY.RDS")
# Lọc dữ liệu
nasdaq_data <- data_intraday %>% filter(codesource == "^VIX") %>% select("codesource","datetime","close")
nasdaq_history <- dbl_source_ins_day_history %>% filter(codesource == "^VIX") %>% select("codesource","date","close") %>% arrange(date)
nasdaq_history [,pclose:=shift(close)]
str(nasdaq_history)
nasdaq_data[,date:=as.Date(datetime)]
nasdaq_data = merge(nasdaq_data[,-c("pclose")], nasdaq_history[,.(date,pclose)], all.x= T,by = "date")
nasdaq_data[,rt:= (close/pclose) -1]
nasdaq_data[, change:= close - pclose]
head(nasdaq_data)
knitr::kable(head(nasdaq_data, 10))
knitr::kable(tail(nasdaq_data, 10))

for (k in 1:10 )
{
  #k=1
  print(k)
  result = -0.01*k
  if( result> 0){
    nasdaq_result= nasdaq_data[rt>=result]
  }else{
    nasdaq_result= nasdaq_data[rt<result]
  }
  #nasdaq_result= nasdaq_data[rt>=result]
  nasdaq_result= unique(nasdaq_result[order(datetime)],by='date')
  knitr::kable(nasdaq_result)
  print(nrow(nasdaq_result))
}
result = -0.08
if( result> 0){
  nasdaq_result= nasdaq_data[rt>=result]
}else{
  nasdaq_result= nasdaq_data[rt<result]
}
#nasdaq_result= nasdaq_data[rt>=result]
nasdaq_result= unique(nasdaq_result[order(datetime)],by='date')
knitr::kable(nasdaq_result)
nrow(nasdaq_result)
uCODE = 'INDSPX'
uDATA = data_intraday[code==uCODE][,.(code, datetime,date, close, open, high, low)]
zDATA = merge(uDATA[,-c("datetime_vix")],nasdaq_result[,.(date,datetime_vix = datetime)], by = c("date"),all.x= T)
zDATA = zDATA[!is.na(datetime_vix)]
zDATA = unique(zDATA[order(date,datetime)][datetime >= datetime_vix], by = c("date"))
zDATA
# uDATA
dDATA = unique(uDATA[order(date,-datetime)], by = c("date"))
fDATA = merge(zDATA[,-c("high_day","low_day")], dDATA[,.(date,high_day= high,low_day = low)], by="date",all.x = T)
if (result>0)
{
  fDATA[, compare:= low_day]
}else
{
  fDATA[,compare:=high_day]
}
fDATA[, rt:=(compare/close)-1]