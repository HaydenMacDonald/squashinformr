% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_player_recent_.R
\name{aggregate_profile_tables}
\alias{aggregate_profile_tables}
\title{Aggregate Profile Tables from Player Profiles}
\usage{
aggregate_profile_tables(x = NULL, y = NULL, identifier = NULL)
}
\arguments{
\item{x, y}{data frames containing}

\item{identifier}{unique string that used to identify the desired table.}
}
\value{
Data frame containing aggregated summary tables.
}
\description{
Given two data frames with profile urls, fetch and aggregate summary tables from player profiles.
}
