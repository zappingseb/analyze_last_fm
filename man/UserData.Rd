% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\docType{class}
\name{UserData}
\alias{UserData}
\title{Last.FM last.year data}
\format{An \code{\link{R6Class}} generator object}
\usage{
data <- UserData$new(username, API_KEY, year, timezone)

album_analysis <- data$albumstats()
}
\arguments{
\item{username}{\code{character} username \code{character} The \url{http://www.last.fm} user whos data shall be taken}

\item{API_KEY}{\code{character} The \url{http://www.last.fm} API key of your app or any app}

\item{year}{\code{integer} Year which shall be analyzed}

\item{timezone}{\code{numeric} Compared time to GMT, for Warsaw or Berlin it's +1}
}
\description{
This class can be used to import data for a specific  \url{http://www.last.fm} user
from a specific year by calling the user.getRecentTracks (\url{https://www.last.fm/api/show/user.getRecentTracks})
function. The whole data will be stored inside the data_table \code{data.frame} of the 
class.
}
\section{Fields}{

\describe{
\item{\code{username}}{\code{character} The \url{http://www.last.fm} user whos data shall be taken}

\item{\code{apikey}}{\code{character} The \url{http://www.last.fm} API key of your app or any app}

\item{\code{timezone}}{\code{numeric} Compared time to GMT, for Warsaw or Berlin it's +1}

\item{\code{data_table}}{\code{data.frame} A data table containing the last years tracks of the
user in the columns
\itemize{
\item{artist:}{ Artist Name}
\item{track:}{ Track Name}
\item{album:}{ Album Name}
\item{uts:}{ Time as a Javascript time starting 1970-01-01}
\item{datetext:}{ Time as Human readible text}
}}
}}

\section{Methods}{

\describe{
  \item{\code{create_api(page)}}{This method uses \code{page} to create the API json call needed for last.fm}
  \item{\code{get_data(year)}}{This method downloads the last.fm data of a specific year into the data_table field}
  \item{\code{clean_data_duplicates()} }{This method will delete scrobbles that are duplicated due to issues of the last.fm / Spotify API}
  \item{\code{album_stats(exclude_artist="Die drei ???",exclude_album="",min_tracks=5,sort_by=c("by_total_count","by_album_count"))}}{
  This method creates a table with album statistics for the specific year}
}
}

\keyword{data}
