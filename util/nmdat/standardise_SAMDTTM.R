#UTC is not a time zone, but a time standard that is the basis for civil time and time zones worldwide. This means 
# that no country or territory officially uses UTC as a local time.

# SAMDTTM:  start Date/time of sampling   TRTSTDTM    TRTSDTM
#adex$SAMDTTM = as.POSIXct(adex$EXSTDT*60*60*24 + adex$EXSTTM, origin="1960-01-01", tz="GMT")
#adex$SAMDTTM = as.POSIXct(adex$TRTSDTM, origin="1960-01-01", tz="GMT")
#adex$SAMDTTM = as.POSIXlt(paste(DATE, "T", TIME1, sep=""), format = "%Y-%m-%dT%H:%M", tz="GMT")
#strptime("Tue, 23 Mar 2010 14:36:38 -0400",  "%a, %d %b %Y %H:%M:%S %z")  

# https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
# "ymd"   "09-01-03 12:02"  "09-01-01", "090102", "09-01 03"
# "ymd HM"  "09-01-03 12:02"  
# x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
# parse_date_time(x, "Ymd HMS", truncated = 3)

# c("Ymd HMS",  "db!Y HMS")  "mdY HMS"



# https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
# "ymd"   "09-01-03 12:02"  "09-01-01", "090102", "09-01 03"
# "ymd HM"  "09-01-03 12:02"  
# x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
# parse_date_time(x, "Ymd HMS", truncated = 3)
#UTC is n ot a time zone, but a time standard that is the basis for civil time and time zones worldwide. This means 
# that no country or territory officially uses UTC as a local time.

# SAMDTTM:  start Date/time of sampling   TRTSTDTM    TRTSDTM
#adex$SAMDTTM = as.POSIXct(adex$EXSTDT*60*60*24 + adex$EXSTTM, origin="1960-01-01", tz="GMT")
#adex$SAMDTTM = as.POSIXct(adex$TRTSDTM, origin="1960-01-01", tz="GMT")
#adex$SAMDTTM = as.POSIXlt(paste(DATE, "T", TIME1, sep=""), format = "%Y-%m-%dT%H:%M", tz="GMT")
#strptime("Tue, 23 Mar 2010 14:36:38 -0400",  "%a, %d %b %Y %H:%M:%S %z")  

# https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
# "ymd"   "09-01-03 12:02"  "09-01-01", "090102", "09-01 03"
# "ymd HM"  "09-01-03 12:02"  
# x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
# parse_date_time(x, "Ymd HMS", truncated = 3)

# SAMDTTM = c("1694336700", "1693386900", "1693818600", 
#             "2013-09-09T09:05:00", "2013-08-30T09:20:00")


standardise_SAMDTTM <- function(SAMDTTM) {
  
  validate(need(SAMDTTM, message="no SAMDTTM in standardise_SAMDTTM"))
  
  SAMDTTM0 = as.character(SAMDTTM)
  SAMDTTM = parse_date_time(SAMDTTM0, orders=timefmt_var_lst, truncated = 3, tz="America/New_York")
  
  # possiblity-1:  sas 10-char integer
  # ---------------------------------------
  # if can't interpretable so far
  ids = which(is.na(SAMDTTM))
  
  if(all(SAMDTTM0[ids] %>% nchar() == 10) && all(!is.na(as_numeric(SAMDTTM0[ids])))) {
    #if("format.sas" %in% names(attributes(SAMDTTM)) &&  attr(SAMDTTM, "format.sas") == "IS8601DT") {
    SAMDTTM[ids] = as.POSIXct(as_numeric(SAMDTTM0[ids]), origin="1960-01-01", tz="America/New_York") 
  } 
  
  #SAMDTTM = as.character(SAMDTTM)
  return(SAMDTTM)
   
}
