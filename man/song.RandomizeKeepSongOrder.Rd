% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/song.randomize.R
\name{song.RandomizeKeepSongOrder}
\alias{song.RandomizeKeepSongOrder}
\title{Generate a randomized performance using the KeepSongOrder method.}
\usage{
song.RandomizeKeepSongOrder(indiv)
}
\arguments{
\item{indiv}{A list created by \code{\link{song.FromDataObj}} or
\code{\link{song.FromTextFile}} that contains the performance statistics of
the individual.}
}
\value{
\code{song.RandomizeKeepSongOrder} returns an n x 2 matrix containing the
start and end times of the songs in the new randomized performance.
}
\description{
\code{song.RandomizeKeepSongOrder} generates a randomized performance based
on an individual's observed singing behavior. The KeepSongOrder method
preserves the individual's natural song and inter-song interval durations as
well as the natural song order, randomizing only the order of the intervals.
This function can be used with \code{\link{song.Simulate}} to calculate the
expected amount of chance overlap in an interaction.
}
\examples{
w <- song.FromDataObj(wrens)
## randomize female's performance
song.RandomizeKeepSongOrder(w$female)
}
\seealso{
Other randomization.functions: \code{\link{song.RandomizeKeepGaps}};
  \code{\link{song.RandomizeSampleGaps}}
}

