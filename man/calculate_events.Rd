% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_events.R
\name{calculate_events}
\alias{calculate_events}
\title{Calculate past and next events given a clean_date}
\usage{
calculate_events(clean_date)
}
\arguments{
\item{clean_date}{Date}
}
\value{
data.table
}
\description{
When entered a `date`, calculate all last events and time until the next 
event. An event is a combination of at least two same values in the 
four-tuple: (year, month, week, day), for instance (1, 0, 1, 1) 
indicating a triple, namely 1 year, 1 week and 1 day soberness.
}
\details{
(x, 0, 0, 0): single
 (0, 0, x, x), (0, x, x, 0), ... : double 
 (0, x, x, x), (x, x, x, 0), ... : triple 
 (x, x, x, x): quartruble
}
\examples{
result <- calculate_events(lubridate::ymd("2012-02-20"))

}
