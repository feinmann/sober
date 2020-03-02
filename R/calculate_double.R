#' Calculate doubles given a clean_date
#' 
#' When entered a `date`, calculate the last double and the next double from the 
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
#' @examples result <- calculate_doubles(lubridate::ymd("2020-02-20"))
calculate_doubles <- function(clean_date) {
  
  number <- 100
  
  dt_list <- list()
  for (i in 1:number) {
    dt_list[[i]] <- data.table(
      event = clean_date %m+% weeks(i) %m+% days(i))
    dt_list[[i]][, message := paste("On ", event, " you will be sober for ", 
                                   i, "weeks and ", 
                                   i, " days.")]
    dt_list[[1]][, message := gsub("weeks", "week", message)]
    dt_list[[1]][, message := gsub("days", "day", message)]
  }
  for (i in 1:number) {
    dt_list[[i+(3*number)]] <- data.table(
      event = clean_date %m+% months(i) %m+% days(i))
    dt_list[[i+(3*number)]][, message := paste("On ", event, " you will be sober for ", 
                                               i, "months and ", 
                                               i, " days.")]
    dt_list[[1+(3*number)]][, message := gsub("months", "month", message)]
    dt_list[[1+(3*number)]][, message := gsub("days", "day", message)]
  }
  for (i in 1:number) {
    dt_list[[i+(3*number)]] <- data.table(
      event = clean_date %m+% years(i) %m+% days(i))
    dt_list[[i+(3*number)]][, message := paste("On ", event, " you will be sober for ", 
                                               i, "years and ", 
                                               i, " days.")]
    dt_list[[1+(3*number)]][, message := gsub("years", "year", message)]
    dt_list[[1+(3*number)]][, message := gsub("days", "day", message)]
  }
  for (i in 1:number) {
    dt_list[[i+(2*number)]] <- data.table(
      event = clean_date %m+% months(i) %m+% weeks(i))
    dt_list[[i+(2*number)]][, message := paste("On ", event, " you will be sober for ", 
                                               i, "months and ", 
                                               i, " weeks.")]
    dt_list[[1+(2*number)]][, message := gsub("months", "month", message)]
    dt_list[[1+(2*number)]][, message := gsub("weeks", "week", message)]
  }
  for (i in 1:number) {
    dt_list[[i+number]] <- data.table(
      event = clean_date %m+% years(i) %m+% weeks(i))
    dt_list[[i+number]][, message := paste("On ", event, " you will be sober for ", 
                                    i, "years and ", 
                                    i, "weeks.")] 
    dt_list[[1+number]][, message := gsub("years", "year", message)]
    dt_list[[1+number]][, message := gsub("weeks", "week", message)]
  }
  for (i in 1:number) {
    dt_list[[i+(3*number)]] <- data.table(
      event = clean_date %m+% years(i) %m+% months(i))
    dt_list[[i+(3*number)]][, message := paste("On ", event, " you will be sober for ", 
                                               i, "years and ", 
                                               i, " months.")]
    dt_list[[1+(3*number)]][, message := gsub("months", "month", message)]
    dt_list[[1+(3*number)]][, message := gsub("years", "year", message)]
  }
  
  dt <- rbindlist(dt_list)
  dt[event <= today(), message := gsub(message, 
                      pattern = "will be", replacement = "were", fixed = TRUE)]
  dt <- dt[order(event)]
  dt[, number := 1:.N]
  dt[, kind := "Double"]
  
  setnames(dt, "event", "Event")
  setnames(dt, "number", "Number")
  setnames(dt, "kind", "Kind")
  setnames(dt, "message", "Message")
  
  return(dt)
}
