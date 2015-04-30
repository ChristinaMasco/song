#' @title Determine whether two songs overlap.
#' 
#' @description
#' \code{song.DoSongsOverlap} determines whether two songs overlap and returns
#' the duration of overlap in seconds.
#' 
#' @param song1,song2 Numeric vectors containing the start and end times 
#' of songs 1 and 2 respectively. The first value in each vector represents the
#' start time of the song; the second value represents the end time of the 
#' song. 
#' 
#' @return \code{song.DoSongsOverlap} returns the duration of overlap in 
#' seconds.
#' 
#' @examples
#' ## overlapping songs
#' song1 <- c(0.0, 1.0)
#' song2 <- c(0.5, 2.0)
#' song.DoSongsOverlap(song1, song2)
#'
#' ## non-overlapping songs
#' song1 <- c(0.0, 1.0)
#' song2 <- c(1.0, 2.0)
#' song.DoSongsOverlap(song1, song2)
#' 
#' @seealso For two performances, use \code{\link{song.TimeOverlap}} to 
#' calculate the total duration of overlap and \code{\link{song.NumOverlap}} to 
#' calculate the total number of overlapping songs. 
#' 

song.DoSongsOverlap <- function(song1, song2){
  overlapping <- 0
  ## if start1 < start2 < end1
  if ((song1[1] <= song2[1]) &&
        (song1[2] > song2[1])){
    overlapping <- 1
  }
  ## if start2 < start1 < end2
  if ((song2[1] <= song1[1]) &&
        (song2[2] > song1[1])) {
    overlapping <- 1
  }
  ## calculate the duration of overlap
  if (overlapping == 1){
    return (min(song1[2], song2[2]) - max(song1[1], song2[1]))
  }
  return(0.0)
}

#' @title Calculate the overlap of one song and a performance.
#' 
#' @description
#' \code{song.OneSongWithSongList} calculates the total duration for which one
#' song overlaps any song in a performance. This function is an extension of 
#' \code{song.DoSongsOverlap}.
#' 
#' @param song A numeric vector containing the start and end times of a song.
#' The first value of the vector represents the start time of the song; the 
#' second value represents the end time of the song.
#' @param songlist An n x 2 matrix or data frame containing the start and end 
#' times of n songs. The first column contains the start times of each song; 
#' the second column contains the end times of each song.
#'  
#' @return
#' \code{song.OneSongWithSongList} returns the total duration of overlap in 
#' seconds.
#' 
#' @examples
#' ## overlapping song
#' song <- c(0.5, 2.0)
#' songlist <- matrix(c(0.0, 1.0, 
#'                      3.0, 4.0, 
#'                      7.0, 8.0,
#'                     10.0, 11.0), ncol=2, byrow=TRUE)
#' song.OneSongWithSongList(song, songlist)
#' 
#' ## non-overlapping song
#' song <- c(1.0, 2.0)
#' songlist <- matrix(c(0.0, 1.0, 
#'                      3.0, 4.0, 
#'                      7.0, 8.0,
#'                     10.0, 11.0), ncol=2, byrow=TRUE)
#' song.OneSongWithSongList(song, songlist)
#' 
#' @seealso \code{\link{song.DoSongsOverlap}}.

song.OneSongWithSongList <- function(song, songlist){
  return(sum(apply(songlist, 1, song.DoSongsOverlap, song)))
}

#' @title Calculate the total duration of overlap.
#' 
#' @description
#' \code{song.TimeOverlap} calculates the total duration for which the songs in
#' two performances overlap. This function is an extension of 
#' \code{song.OneSongWithSongList}.
#' 
#' @param songlist1,songlist2 n x 2 matrices or data frames containing the 
#' start and end times of n songs. The first columns contain the start times of
#' each song; the second columns contain the end times of each 
#' song.
#' 
#' @return \code{song.TimeOverlap} returns the total duration of overlap in 
#' seconds.
#' 
#' @examples
#' songlist1 <- matrix(c(0.0, 1.0, 
#'                       3.0, 4.0, 
#'                       7.0, 8.0,
#'                      10.0, 11.0), ncol=2, byrow=TRUE)
#' songlist2 <- matrix(c(0.5, 1.5, 
#'                       4.0, 5.0, 
#'                       6.5, 7.5,
#'                      10.5, 12.0), ncol=2, byrow=TRUE)
#' song.TimeOverlap(songlist2, songlist1)
#' 
#' @seealso \code{\link{song.DoSongsOverlap}} for comparing two songs,
#' \code{\link{song.OneSongWithSongList}} for comparing one song to a 
#' performance, and \code{\link{song.NumOverlap}} for calculating the 
#' \emph{number} of overlapping songs rather than duration of overlap.

song.TimeOverlap <- function(songlist2, songlist1){
  overlaps <- apply(songlist1, 1, song.OneSongWithSongList, songlist2)
  return(sum(overlaps))
}

#' @title Count the total number of songs that overlap.
#' 
#' @description
#' \code{song.NumOverlap} counts the total number of overlapping songs in two 
#' performances. This function is an extension of 
#' \code{song.OneSongWithSongList}.
#' 
#' @param songlist1,songlist2 n x 2 matrices or data frames containing the 
#' start and end times of n songs. The first columns contain the start times of
#' each song; the second columns contain the end times of each song.
#' 
#' @return \code{song.NumOverlap} returns the total number of songs that 
#' overlap.
#' 
#' @examples
#' songlist1 <- matrix(c(0.0, 1.0, 
#'                       3.0, 4.0, 
#'                       7.0, 8.0,
#'                      10.0, 11.0), ncol=2, byrow=TRUE)
#' songlist2 <- matrix(c(0.5, 1.5, 
#'                       4.0, 5.0, 
#'                       6.5, 7.5,
#'                      10.5, 12.0), ncol=2, byrow=TRUE)
#' song.NumOverlap(songlist2, songlist1)
#' 
#' @seealso \code{\link{song.DoSongsOverlap}} for comparing two songs,
#' \code{\link{song.OneSongWithSongList}} for comparing one song to a 
#' performance, and \code{\link{song.TimeOverlap}} for calculating the 
#' \emph{duration} of overlap rather than the number of overlapping songs.

song.NumOverlap <- function(songlist2, songlist1){
  overlaps <- apply(songlist1, 1, song.OneSongWithSongList, songlist2)
  return(sum(overlaps > 0))
}
