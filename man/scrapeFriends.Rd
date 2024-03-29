% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALLFUNCTIONS.R
\name{scrapeFriends}
\alias{scrapeFriends}
\title{Scrape Friends of a Panel}
\usage{
scrapeFriends(
  panel_directory,
  list_tokens,
  n = 20000,
  per_token_limit = 15,
  max_hours = 15
)
}
\arguments{
\item{panel_directory}{The path to the panel folder corresponding to the set of users to be scraped.}

\item{list_tokens}{The list of tokens to be used for scraping.  See prepTokens().}

\item{n}{The maximum number of friends to scrape for each user.  Defaults to 20,000}

\item{max_hours}{The maximum number of hours to continue scraping.  Defaults to 1 hour.}
}
\description{
A high-level function to scrape friends of users in a given panel, and save the resulting data in that panel folder.
}
\examples{
scrapeFriends()
}
\keyword{scraping}
