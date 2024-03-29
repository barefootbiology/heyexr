% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anonymize_dates.R
\name{anonymize_dates}
\alias{anonymize_dates}
\title{Anonymize dates in a volume object.}
\usage{
anonymize_dates(
  header,
  anon_dob = as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
)
}
\arguments{
\item{header}{The header list from a volume object.}

\item{anon_dob}{The new date of birth. Must be at or after the UNIX date-time
origin of 1970-01-01 UTC (the default value).}
}
\value{
A new header list with the dates adjusted.
}
\description{
\code{anonymize_dates} sets the date of birth ('dob') to a given reference
point then adjusts the 'exam_time' and 'visit_date' values to preserve the
age at visit.
}
\keyword{internal}
