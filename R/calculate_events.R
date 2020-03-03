#' Calculate past and next events given a clean_date
#' 
#' When entered a `date`, calculate all last events and time until the next 
#' event. An event is a combination of at least two same values in the 
#' four-tuple: (year, month, week, day), for instance (1, 0, 1, 1) 
#' indicating a triple, namely 1 year, 1 week and 1 day soberness.
#' 
#'  (0, 0, x, x): double 
#'  (0, x, x, x): triple 
#'  (x, x, x, x): quartuble 
#'
#' @param clean_date Date
#'
#' @return data.table
#' @export
#' 
#' @import lubridate
#' @import data.table
#'
#' @examples result <- calculate_events(lubridate::ymd("2012-02-20"))
#' 
calculate_events <- function(clean_date) {
  
  max_double <- ceiling((today() - clean_date)/(dweeks(1) + ddays(1)))
  max_triple <- ceiling((today() - clean_date)/(dweeks(4) + dweeks(1) + ddays(1)))
  max_year <- ceiling((today() - clean_date)/(dyears(1)))
  
  tmp <- data.table(expand.grid(0:max_year, 0:max_triple, 0:max_double, 0:max_double))
  names(tmp) <- c("year", "month", "week", "day")
  
  tmp <- rbindlist(
    list(
      tmp[year==month, ],
      tmp[year==week, ],
      tmp[year==day, ],
      tmp[month==week, ],
      tmp[month==day, ],
      tmp[week==day, ]
    )
  )
  
  tmp <- unique(tmp)
  
  tmp[, event := clean_date %m+% years(year) %m+% months(month) %m+% weeks(week) %m+% days(day)]
  
  tmp <- tmp[order(event)]
  
  tmp[year == month & week == 0 & day == 0, kind := "double"]
  tmp[year == week & month == 0 & day == 0, kind := "double"]
  tmp[year == day & week == 0 & month == 0, kind := "double"]
  tmp[month == week & year == 0 & day == 0, kind := "double"]
  tmp[month == day & week == 0 & year == 0, kind := "double"]
  tmp[week == day & month == 0 & year == 0, kind := "double"]

  tmp[year == month & month == week & day == 0, kind := "triple"]
  tmp[year == month & month == day & week == 0, kind := "triple"]
  tmp[year == day & day == week & month == 0, kind := "triple"]
  tmp[month == day & day == week & year == 0, kind := "triple"]
  
  tmp[year == month & month == week & week == day, kind := "quartuble"]
  tmp[year == 0 & month == 0 & week == 0 & day == 0, kind := NA_character_]
  
  tmp <- tmp[!is.na(kind), ]

  return(tmp)
}


get_next_events <- function(events) {
  events[event > lubridate::today(), head(.SD, 1), by=.(kind)][, .(event, kind)]
}
