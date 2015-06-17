#' @title Determine whether two songs overlap.
#' 
#' @description
#' \code{song.DoSongsOverlap} determines whether two songs overlap and returns
#' the duration of overlap in seconds. The sign of this value indicates the 
#' order of the overlapping songs.
#' 
#' @param song1,song2 Numeric vectors containing the start and end times 
#' of songs 1 and 2 respectively. The first value in each vector represents the
#' start time of the song; the second value represents the end time of the 
#' song. 
#' 
#' @return \code{song.DoSongsOverlap} returns the duration of overlap in 
#' seconds. If \code{song2} begins during \code{song1} (i.e.\code{song1} is 
#' leading), the function returns a positive value. If \code{song1} begins 
#' during \code{song2} (i.e. \code{song2} is leading), the function returns a 
#' negative value. If the songs are non-overlapping, the function returns a 
#' value of 0. 
#' 
#' @examples
#' ## songs overlap and song1 is leading
#' song1 <- c(0.0, 1.0)
#' song2 <- c(0.5, 2.0)
#' song.DoSongsOverlap(song1,song2)
#' 
#' ## songs overlap and song1 is lagging
#' song1 <- c(1.0, 3.0)
#' song2 <- c(0.0, 2.0)
#' song.DoSongsOverlap(song1,song2)
#' 
#' ## songs do not overlap
#' song1 <- c(0.0, 1.0)
#' song2 <- c(2.0, 3.0)
#' song.DoSongsOverlap(song1,song2)
#' 
#' @seealso For two performances, use \code{\link{song.TimeOverlap}} to 
#' calculate the total duration of overlap and \code{\link{song.NumOverlap}} to 
#' calculate the total number of overlapping songs. 
#' 

song.DoSongsOverlap <- function(song1, song2){
  overlapping <- 0
  ## if start1 < start2 < end1 (song 2 is lagging)
  if ((song1[1] <= song2[1]) &&
        (song1[2] > song2[1])){
    overlapping <- 1
  }
  ## if start2 < start1 < end2 (song 1 is lagging)
  if ((song2[1] <= song1[1]) &&
        (song2[2] > song1[1])) {
    overlapping <- -1
  }
  ## calculate the duration of overlap
  if (overlapping == 1 | overlapping == -1){
    return ((min(song1[2], song2[2]) - max(song1[1], song2[1])) * overlapping)
  }
  return(0.0)
}

#' @title Calculate the overlap of one song and a performance.
#' 
#' @description
#' \code{song.OneSongWithSongList} calculates the duration for which one
#' song overlaps and lags behind any song in a performance. This function is an
#' extension of \code{\link{song.DoSongsOverlap}}.
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

song.OneSongWithSongList <- function(song, songlist){
  overlaps <- apply(songlist, 1, song.DoSongsOverlap, song)
  ## sum positive overlaps only (song overlaps and lags behind)
  return(sum(overlaps[which(overlaps > 0)]))
}

#' @title Calculate the total duration of overlap.
#' 
#' @description
#' \code{song.TimeOverlap} calculates the total duration for which the songs in
#' one performance overlap and lag behind the songs in another. This function 
#' is an extension of \code{\link{song.OneSongWithSongList}}.
#' 
#' @param target,reference n x 2 matrices or data frames containing the start
#' and end times of n songs. The first columns contain the start times of each 
#' song; the second columns contain the end times of each song. The amount of 
#' overlap is calculated for the \code{target} with respect to the 
#' \code{reference}.
#' 
#' @return \code{song.TimeOverlap} returns the total duration of overlap in 
#' seconds.
#' 
#' @examples
#' reference <- matrix(c(0.0, 1.0, 
#'                       3.0, 4.0, 
#'                       7.0, 8.0,
#'                      10.0, 11.0), ncol=2, byrow=TRUE)
#' target <- matrix(c(0.5, 1.5, 
#'                      4.0, 5.0, 
#'                      6.5, 7.5,
#'                     10.5, 12.0), ncol=2, byrow=TRUE)
#' song.TimeOverlap(target, reference)
#' 
#' @seealso \code{\link{song.DoSongsOverlap}} for comparing two songs,
#' \code{\link{song.OneSongWithSongList}} for comparing one song to a 
#' performance, and \code{\link{song.NumOverlap}} for calculating the 
#' \emph{number} of overlapping songs rather than duration of overlap.

song.TimeOverlap <- function(target, reference){
  overlaps <- apply(target, 1, song.OneSongWithSongList, reference)
  ## sum positive overlaps only (target overlaps and lags behind reference)
  return (sum(overlaps[which(overlaps > 0)]))
}

#' @title Count the total number of songs that overlap.
#' 
#' @description
#' \code{song.NumOverlap} counts the total number of songs in one performance
#' that overlap and lag behind the songs in another. This function is an extension of 
#' \code{\link{song.OneSongWithSongList}}.
#' 
#' @param target,reference n x 2 matrices or data frames containing the start
#' and end times of n songs. The first columns contain the start times of each 
#' song; the second columns contain the end times of each song. The amount of 
#' overlap is calculated for the \code{target} with respect to the 
#' \code{reference}.
#' 
#' @return \code{song.NumOverlap} returns the total number of songs that 
#' overlap.
#' 
#' @examples
#' reference <- matrix(c(0.0, 1.0, 
#'                       3.0, 4.0, 
#'                       7.0, 8.0,
#'                      10.0, 11.0), ncol=2, byrow=TRUE)
#' target <- matrix(c(0.2, 1.5, 
#'                      4.0, 5.0, 
#'                      6.5, 7.5,
#'                     10.5, 12.0), ncol=2, byrow=TRUE)
#' song.NumOverlap(target, reference)
#' 
#' @seealso \code{\link{song.DoSongsOverlap}} for comparing two songs,
#' \code{\link{song.OneSongWithSongList}} for comparing one song to a 
#' performance, and \code{\link{song.TimeOverlap}} for calculating the 
#' \emph{duration} of overlap rather than the number of overlapping songs.

song.NumOverlap <- function(target, reference){
  overlaps <- apply(target, 1, song.OneSongWithSongList, reference)
  ## count positive overlaps only (target overlaps and lags behind reference)
  return (sum(overlaps > 0))
}
