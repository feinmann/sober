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
  for (i in 1:100) {
    dt_list[[i]] <- data.table(
      clean_date = clean_date,
      triple = clean_date %m+% months(i) %m+% weeks(i) %m+% days(i),
      triple_nr = i)
  }
  dt <- rbindlist(dt_list)
  dt[, `What about triples?` := paste("On ", triple, " you will be sober for ", 
                                        triple_nr, "months, ", 
                                        triple_nr, "weeks and ", 
                                        triple_nr, " days.")]
  dt[triple_nr == 1, `What about triples?` := paste("On ", triple, " you will be sober for ", 
                                                    triple_nr, "month, ", 
                                                    triple_nr, "week and ", 
                                                    triple_nr, " day.")]
  dt[triple <= today(), `What about triples?` := gsub(`What about triples?`, 
                                        pattern = "will be", replacement = "were", fixed = TRUE)]
  return(dt)
}
