#' @title Determine whether two songs overlap
#' 
#' @description
#' \code{song.DoSongsOverlap} determines whether two songs overlap, and returns
#' the duration of overlap in seconds
#' 
#' @param my.song1,mysong.2 Numeric vectors containing the start and end times
#'  of songs 1 and 2 respectively 
#' 
#' @return The amount of time (in seconds) for which the two songs overlap
#' @examples
#' s1 <- c(0.0, 1.0)
#' s2 <- c(0.5, 2.0)
#' print(song.DoSongsOverlap(s1, s2))
#'
#' s1 <- c(0.0, 1.0)
#' s2 <- c(1.0, 2.0)
#' print(song.DoSongsOverlap(s1, s2))
song.DoSongsOverlap <- function(my.song1, my.song2){
  ## if the two songs are overlapping, return the overlap time
  ## otherwise, return 0
  overlapping <- 0
  ## if start1 < start2 and end1 > start2
  if ((my.song1[1] <= my.song2[1]) &&
        (my.song1[2] >= my.song2[1])){
    overlapping <- 1
  }
  ## if start2 < start1 and end2 > start1
  if ((my.song2[1] <= my.song1[1]) &&
        (my.song2[2] >= my.song1[1])) {
    overlapping <- 1
  }
  if (overlapping == 1){
    return (min(my.song1[2], my.song2[2]) - max(my.song1[1], my.song2[1]))
  }
  return(0.0)
}

song.OverlapOneSongWithSongList <- function(my.song, my.songlist){
  ## total overlap between my.song and all songs in my.songlist
  return(sum(apply(my.songlist, 1, song.DoSongsOverlap, my.song)))
}

song.TimeOverlap <- function(songlist.2, songlist.1){
  ## Compute the overlap in seconds between the songs
  ## of the first bird (stored in songlist.1) and the
  ## second bird (songlist.2)
  overlaps <- apply(songlist.1, 1, song.OverlapOneSongWithSongList, songlist.2)
  return(sum(overlaps))
}

song.NumOverlap <- function(songlist.2, songlist.1){
  ## Compute the number of songs in songlist.1
  ## which overlap with any song in songlist.2
  overlaps <- apply(songlist.1, 1, song.OverlapOneSongWithSongList, songlist.2)
  return(sum(overlaps > 0))
}
