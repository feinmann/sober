#' Calculate triples given a clean_date
#' 
#' When entered a `date`, calculate the last triple and the next triple from the 
#' time-distance between today and `date`, that is a combination t of same 
#' numbers for (month, week, day), for example (1, 1, 1) for the time-distance 
#' from `date` until today of 1 month, 1 week and 1 day.
#'
#' @param clean_date Date
#'
#' @return data.table
#' @export
#' @import lubridate
#' @import data.table
#'
#' @examples result <- calculate_triple(lubridate::ymd("2020-02-20"))
calculate_triples <- function(clean_date) {

  dt_list <- list()
  for (i in 1:200) {
    dt_list[[i]] <- data.table(
      clean_date = clean_date,
      triple = clean_date %m+% months(i) %m+% weeks(i) %m+% days(i),
      triple_nr = i)
  }
  data <- rbindlist(dt_list)
  data[, `What about triples?` := paste("On ", triple, " you are sober for ", 
                                        triple_nr, "months, ", 
                                        triple_nr, "weeks and ", 
                                        triple_nr, " days.")]
  return(data)
}