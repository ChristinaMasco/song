% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/song.summarize.R
\name{song.Summarize}
\alias{song.Summarize}
\title{Summarize the results of a simulation.}
\usage{
song.Summarize(results)
}
\arguments{
\item{results}{A list created by \code{\link{song.Simulate}} that contains
the observed and expected amounts of overlap for an interaction.}
}
\value{
\code{song.Summarize} returns a data frame containing the observed
amount of overlap, the expected amount of overlap, and the resulting p-value
for each possible pair of individuals. Overlap is measured for the target
individual with respect to the reference individual.
}
\description{
\code{song.Summarize} creates a data frame summarizing the output of
\code{\link{song.Simulate}}.
}
\examples{
w <- song.FromDataObj(wrens)
w.rand <- song.Simulate(w, 100, song.TimeOverlap,
                        song.RandomizeKeepSongOrder)
song.Summarize(w.rand)
}

