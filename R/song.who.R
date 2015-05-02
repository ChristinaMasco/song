#' @title Determine the direction of song overlap.
#' 
#' @description
#' \code{song.WhichSongsOverlap} determines whether two songs overlap and 
#' assigns a value (1 or -1) to indicate the order of the overlapping songs. 
#'
#' @param song1,song2 Numeric vectors containing the start and end times 
#' of songs 1 and 2 respectively. The first value in each vector represents the
#' start time of the song; the second value represents the end time of the 
#' song.  
#'
#' @return 
#' If \code{song2} begins during \code{song1} (i.e.\code{song1} is leading), 
#' the function returns a value of 1. If \code{song1} begins during 
#' \code{song2} (i.e. \code{song2} is leading), the function returns a value of
#' -1. If the songs are non-overlapping, the function returns a value of 0. 
#' 
#' @examples
#' ## songs overlap and song1 is leading
#' song1 <- c(0.0, 1.0)
#' song2 <- c(0.5, 2.0)
#' song.WhichSongsOverlap(song1,song2)
#' 
#' ## songs overlap and song1 is lagging
#' song1 <- c(1.0, 3.0)
#' song2 <- c(0.0, 2.0)
#' song.WhichSongsOverlap(song1,song2)
#' 
#' ## songs do not overlap
#' song1 <- c(0.0, 1.0)
#' song2 <- c(2.0, 3.0)
#' song.WhichSongsOverlap(song1,song2)
#' 
#' @seealso \code{\link{song.CountWhichOverlap}} for counting instances of 
#' overlap given two performances, and \code{\link{song.WhoOverlaps}} for 
#' counting instances of overlap by individual.

song.WhichSongsOverlap <- function(song1, song2){
  ## if songs do not overlap, return 0
  overlapping <- 0
  ## if start1 < start2 < end1
  if ((song1[1] <= song2[1]) &&
        (song1[2] > song2[1])){
    overlapping <- 1
  }
  ## if start2 < start1 < end2 
  if ((song2[1] <= song1[1]) &&
        (song2[2] > song1[1])) {
    overlapping <- -1
  }
  return(overlapping)
}

#' @title Determine the direction of overlap for one song and a performance.
#' 
#' @description
#' \code{song.WhichSongWithSongList} determines whether a song overlaps and 
#' lags behind any song in a given performance. This function is an extension 
#' of \code{song.WhichSongsOverlap}.
#' 
#' @param song A numeric vector containing the start and end times of a song.
#' The first value of the vector represents the start time of the song; the 
#' second value represents the end time of the song.
#' @param songlist An n x 2 matrix or data frame containing the start and end 
#' times of n songs. The first column contains the start times of each song; 
#' the second column contains the end times of each song.
#'
#' @return 
#' If \code{song} begins during any song in \code{songlist}, the function 
#' returns a value of 1. Otherwise (i.e. if there is no overlap, or overlapping
#' occurs in the opposite direction), the function returns a value of 0. 
#' 
#' @examples
#' ## song overlaps and lags behind a song in songlist
#' song <- c(0.5, 2.0)
#' songlist <- matrix(c(0.0, 1.0, 
#'                      3.0, 4.0, 
#'                      7.0, 8.0,
#'                     10.0, 11.0), ncol=2, byrow=TRUE)
#' song.WhichSongWithSongList(song, songlist)
#' 
#' ## song overlaps but starts before a song in songlist
#' song <- c(2.5, 4.0)
#' songlist <- matrix(c(0.0, 1.0, 
#'                      3.0, 4.0, 
#'                      7.0, 8.0,
#'                     10.0, 11.0), ncol=2, byrow=TRUE)
#' song.WhichSongWithSongList(song, songlist)
#' 
#' @seealso \code{\link{song.WhichSongsOverlap}}

song.WhichSongWithSongList <- function(song, songlist){
  overlaps <- apply(songlist, 1, song.WhichSongsOverlap, song)
  return(sum(overlaps > 0))
}

#' @title Count instances of overlap given two performances.
#' 
#' @description
#' \code{song.CountWhichOverlap} counts the number of instances in which songs
#' in one performance lag behind and overlap songs in another performance. This
#' function is an extension of \code{song.WhichSongWithSonglist}. 
#' 
#' @param songlist,reference n x 2 matrices or data frames containing the start
#' and end times of n songs. The first columns contain the start times of each 
#' song; the second columns contain the end times of each song.
#'
#' @return \code{song.CountWhichOverlap} returns the total number of instances
#' in which songs in \code{songlist} lag behind and overlap songs in the 
#' reference songlist, (\code{reference}). 
#' 
#' @examples
#' reference <- matrix(c(0.0, 1.0, 
#'                       3.0, 4.0, 
#'                       7.0, 8.0,
#'                      10.0, 11.0), ncol=2, byrow=TRUE)
#' songlist <- matrix(c(0.5, 1.5, 
#'                      4.0, 5.0, 
#'                      6.5, 7.5,
#'                     10.5, 12.0), ncol=2, byrow=TRUE)
#' song.CountWhichOverlap(songlist, reference)
#' 
#' @seealso \code{\link{song.WhichSongsOverlap}} and 
#' \code{\link{song.WhichSongWithSongList}} for determining the direction of
#' overlap

song.CountWhichOverlap <- function(songlist, reference){
  overlaps <- apply(songlist, 1, song.WhichSongWithSongList, reference)
  return (sum(overlaps))
}


#' @title Count the instances of overlap by individual.
#' 
#' @description
#' \code{song.WhoOverlaps} counts the number of instances in which each 
#' individual overlaps each other individual's songs. This function is an
#' extension of \code{song.CountWhichOverlap}.
#' 
#' @param birds A list created using \code{song.BuildSongList} or 
#' \code{song.ReadSongList} that contains the performances of each individual. 
#'  
#' @return \code{song.WhoOverlaps} returns a data frame containing the number 
#' of instances of overlap for each pairwise interaction, specifying the 
#' "overlappers" and reference individuals in each case.
#' 
#' @examples
#' w <- song.BuildSongList(wrens)
#' song.WhoOverlaps(w)
#' 
#' @seealso \code{\link{song.CountWhichOverlap}}

song.WhoOverlaps <- function(birds){
  id <- names(birds)
  overlaps <- data.frame(overlapper=character(), reference=character(), num.overlap=numeric())
  for(i in id){
    for(j in id){
      num <- song.CountWhichOverlap(birds[[i]]$songs, birds[[j]]$songs)
      overlaps <- rbind(overlaps, data.frame(overlapper = i, 
                                             reference = j, num.overlap = num))
    }    
  }
  overlaps <- overlaps[which(overlaps$overlapper != overlaps$reference),]
  return(overlaps)
}