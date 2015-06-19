#' @title Generate a randomized performance using the SampleGaps method.
#'
#' @description
#' \code{song.RandomizeSampleGaps} generates a randomized performance based on
#' an individual's observed singing behavior. The SampleGaps method preserves
#' the individual's natural song durations, while randomizing both the order of
#' the songs and the duration of the inter-song intervals. This function can be
#' used with \code{\link{song.Simulate}} to calculate the expected amount of
#' chance overlap in an interaction.
#'
#' @param indiv A list created using \code{\link{song.BuildAllIndivs}} or
#' \code{\link{song.ReadSongList}} that contains the performance statistics of
#' the individual.
#'
#' @return
#' \code{song.RandomizeSampleGaps} returns an n x 2 matrix containing the start
#' and end times of the songs in the new randomized performance.
#'
#' @examples
#' c <- song.BuildAllIndivs(chickadees)
#' song.RandomizeSampleGaps(c$bird)
#'
#' @family randomization functions
#' @export
song.RandomizeSampleGaps <- function(indiv){
  rnd.songs <- indiv$songs
  ## randomize song order
  rnd.songs.length <- sample(indiv$songs.length)
  ## randomly select interval duration, keeping total duration constant
  break.points <- c(0,
                    sort(runif(indiv$songs.num)) * indiv$gaps.total.length,
                    indiv$gaps.total.length)
  rnd.gaps.length <- break.points[2:length(break.points)] -
    break.points[1:(length(break.points) - 1)]
  ## calculate the total duration
  cumul.gaps <- cumsum(rnd.gaps.length[-length(rnd.gaps.length)])
  cumul.songs <- cumsum(rnd.songs.length)
  ## calculate the end of the last song (offset by start.record.time)
  end.songs <- cumul.gaps + cumul.songs + indiv$start.record.time
  ## calculate the start of the first song
  start.songs <- end.songs - rnd.songs.length
  rnd.songs[,1] <- start.songs
  rnd.songs[,2] <- end.songs
  return(rnd.songs)
}

#' @title Generate a randomized performance using the KeepGaps method.
#'
#' @description
#' \code{song.RandomizeKeepGaps} generates a randomized performance based on
#' an individual's observed singing behavior. The KeepGaps method preserves the
#' individual's natural song and inter-song interval durations, while
#' randomizing the order of the songs and intervals. This function can be used
#' with \code{\link{song.Simulate}} to calculate the expected amount of chance
#' overlap in an interaction.
#'
#' @param indiv A list created using \code{\link{song.BuildAllIndivs}} or
#' \code{\link{song.ReadSongList}} that contains the performance statistics of
#' the individual.
#'
#' @return
#' \code{song.RandomizeKeepGaps} returns an n x 2 matrix containing the start
#' and end times of the songs in the new randomized performance.
#'
#' @examples
#' m <- song.BuildAllIndivs(manakins)
#' song.RandomizeKeepGaps(m$pairA)
#'
#' @family randomization functions
#' @export
song.RandomizeKeepGaps <- function(indiv){
  rnd.songs <- indiv$songs
  ## randomize song and interval order
  rnd.songs.length <- sample(indiv$songs.length)
  rnd.gaps.length <- sample(indiv$gaps.length)
  ## calculate the total duration
  cumul.gaps <- cumsum(rnd.gaps.length[-length(rnd.gaps.length)])
  cumul.songs <- cumsum(rnd.songs.length)
  ## calculate the end of the last song (offset by start.record.time)
  end.songs <- cumul.gaps + cumul.songs + indiv$start.record.time
  ## calculate the start of the first song
  start.songs <- end.songs - rnd.songs.length
  rnd.songs[,1] <- start.songs
  rnd.songs[,2] <- end.songs
  return(rnd.songs)
}

#' @title Generate a randomized performance using the KeepSongOrder method.
#'
#' @description
#' \code{song.RandomizeKeepSongOrder} generates a randomized performance based
#' on an individual's observed singing behavior. The KeepSongOrder method
#' preserves the individual's natural song and inter-song interval durations as
#' well as the natural song order, randomizing only the order of intervals.
#' This function can be used with \code{\link{song.Simulate}} to calculate the
#' expected amount of chance overlap in an interaction.
#'
#' @param indiv A list created using \code{\link{song.BuildAllIndivs}} or
#' \code{\link{song.ReadSongList}} that contains the performance statistics of
#' the individual.
#'
#' @return
#' \code{song.RandomizeKeepSongOrder} returns an n x 2 matrix containing the
#' start and end times of the songs in the new randomized performance.
#'
#' @examples
#' w <- song.BuildAllIndivs(wrens)
#' song.RandomizeKeepSongOrder(w$female)
#'
#' @family randomization functions
#' @export
song.RandomizeKeepSongOrder <- function(indiv){
  rnd.songs <- indiv$songs
  ## keep the observed song order, randomize interval order
  rnd.songs.length <- indiv$songs.length
  rnd.gaps.length <- sample(indiv$gaps.length)
  ## calculate total duration
  cumul.gaps <- cumsum(rnd.gaps.length[-length(rnd.gaps.length)])
  cumul.songs <- cumsum(rnd.songs.length)
  ## calculate the end of the last song (offset by start.record.time)
  end.songs <- cumul.gaps + cumul.songs + indiv$start.record.time
  ## calculate the start of the first song
  start.songs <- end.songs - rnd.songs.length
  rnd.songs[,1] <- start.songs
  rnd.songs[,2] <- end.songs
  return(rnd.songs)
}
