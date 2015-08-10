#' @title Build a list of performance statistics for an individual.
#'
#' @description
#' \code{song.BuildIndiv} builds a list of performance statistics for an
#' individual based on the start and end times of the individual's songs.
#'
#' @param ID A character string giving the name of the individual.
#' @param songs An n x 2 matrix or data frame containing the start and end
#' times of n songs. The first column contains the start times of each song;
#' the second column contains the end times of each song.
#' @param start.record.time A numeric value indicating the start time of the
#' performance.
#' @param end.record.time A numeric value indicating the end time of the
#' performance.
#'
#' @return
#' \code{song.BuildIndiv} returns a list containing the following components:
#' \describe{
#'   \item{\code{ID}}{A character string giving the name of the individual.}
#'   \item{\code{start.record.time}}{A numeric value indicating the start time
#'   of the performance.}
#'   \item{\code{end.record.time}}{A numeric value indicating the end time of
#'   the performance.}
#'   \item{\code{songs}}{A matrix containing the start and end times of
#'   the songs.}
#'   \item{\code{songs.num}}{An integer value indicating the number of songs
#'   in the performance.}
#'   \item{\code{songs.length}}{A named numeric vector containing the durations
#'   of each song in the performance.}
#'   \item{\code{songs.total.length}}{A numeric value indicating the total time
#'   spent singing (i.e. \code{sum(songs.length)}).}
#'   \item{\code{gaps.num}}{An integer value indicating the number of
#'   inter-song intervals in the performance.}
#'   \item{\code{gaps.length}}{A named numeric vector containing the durations
#'   of each inter-song interval in the performance.}
#'   \item{\code{gaps.total.length}}{A numeric value indicating the total time
#'   spent silent (i.e. \code{sum(gaps.length)}).}
#' }
#'
#' @examples
#' songs <- matrix(c(0.0, 1.0,
#'                   3.0, 4.0,
#'                   7.0, 8.0,
#'                  10.0, 11.0), ncol=2, byrow=TRUE)
#' bird <- song.BuildIndiv(ID = "bird", songs, start.record.time = 0.0,
#'                         end.record.time = 12.0)
#'
#' @seealso
#' To extract performance statistics from interactions involving multiple
#' individuals, use \code{\link{song.FromDataObj}} (for data stored in a matrix
#' or data frame) or \code{\link{song.FromTextFile}} (for data stored in a
#' tab-delimited text file.
#' @export

song.BuildIndiv <- function(ID, songs, start.record.time, end.record.time){
  indiv <- list()
  ## indiv identifier
  indiv$ID <- ID
  ## info on the recording time
  indiv$start.record.time <- start.record.time
  indiv$end.record.time <- end.record.time
  ## songs [start, end] for each song
  colnames(songs) <- c("start", "end")
  rownames(songs) <- 1:dim(songs)[1]
  indiv$songs <- songs
  ## number of songs
  indiv$songs.num <- dim(songs)[1]
  ## duration of each song
  indiv$songs.length <- songs[,2] - songs[,1]
  ## total time spent singing
  indiv$songs.total.length <- sum(indiv$songs.length)
  ## inter-song intervals (gaps)
  for.gaps.start <- c(songs[,1], end.record.time)
  for.gaps.end <- c(start.record.time, songs[,2])
  indiv$gaps.num <- indiv$songs.num + 1
  indiv$gaps.length <- for.gaps.start - for.gaps.end
  indiv$gaps.total.length <- sum(indiv$gaps.length)
  ## check: gaps must be of positive length
  if (sum(indiv$gaps.length < 0.) > 0){
    print(paste(indiv$ID, "has inter-song intervals of negative length!"))
    ## print rows with negative gap lengths
    start <- as.vector(indiv$songs[,1])
    end <- as.vector(indiv$songs[,2])
    interval <- head(as.vector(indiv$gaps.length),-1)
    output <- data.frame(start,end,interval)
    print(output[which(output$interval<0),], row.names=TRUE)
    return(output)
  }
  return(indiv)
}

#' @title Extract performance statistics from a text file.
#'
#' @description
#' \code{song.FromTextFile} builds a list of performance statistics for each
#' individual from a tab-delimited text file containing the start time, end
#' time, and singer identity for each song in an interaction. This function is
#' an extension of \code{\link{song.BuildIndiv}}.
#'
#' @details
#' The input for this function is a tab-delimited text file (*.txt) containing
#' the start time, end time, and singer identity for each song during an
#' interaction. These data can be acquired using sound annotation software
#' packages such as \href{http://www.syrinxpc.com/}{Syrinx-PC} (J. Burt,
#' Seattle, WA, USA) or
#' \href{http://www.birds.cornell.edu/brp/raven/RavenOverview.html}{Raven}
#' (Cornell Laboratory of Ornithology, Ithaca, NY USA).
#' The following is an example input file:
#' \tabular{lll}{
#' start \tab end \tab ID \cr
#' 0 \tab 5 \tab bird1 \cr
#' 2 \tab 8 \tab bird2 \cr
#' 10 \tab 15 \tab bird1 \cr
#' 12 \tab 16 \tab bird2
#' }
#'
#' @param file The name of the file from which the data are to be read. If it
#' does not contain an absolute path, the file name is relative to the current
#' working directory.
#' @param start.record.time A numeric value indicating the start time of the
#' interaction. An optional parameter - if not specified, the minimum start
#' time is used (i.e. the start of the first song).
#' @param end.record.time A numeric value indicating the end time of the
#' interaction. An optional parameter - if not specified, the maximum end time
#' is used (i.e. the end of the last song).
#'
#' @return
#' \code{song.FromTextFile} returns a list containing the following components
#' for each individual:
#' \describe{
#'   \item{\code{ID}}{A character string giving the name of the individual.}
#'   \item{\code{start.record.time}}{A numeric value indicating the start time
#'   of the interaction.}
#'   \item{\code{end.record.time}}{A numeric value indicating the end time of
#'   the interaction.}
#'   \item{\code{songs}}{A matrix containing the start and end times of
#'   each of the individual's songs.}
#'   \item{\code{songs.num}}{An integer value indicating the number of songs
#'   produced by the individual.}
#'   \item{\code{songs.length}}{A named numeric vector containing the durations
#'   of each song produced by the individual.}
#'   \item{\code{songs.total.length}}{A numeric value indicating the total time
#'   for which the individual was singing (i.e. \code{sum(songs.length)}).}
#'   \item{\code{gaps.num}}{An integer value indicating the number of
#'   inter-song intervals in the individual's performance.}
#'   \item{\code{gaps.length}}{A named numeric vector containing the durations
#'   of each inter-song interval in the individual's performance.}
#'   \item{\code{gaps.total.length}}{A numeric value indicating the total time
#'   for which the individual was silent (i.e. \code{sum(gaps.length)}).}
#' }
#'
#' @seealso \code{\link{song.FromDataObj}} for data stored in a matrix or
#' data frame.
#' @export

song.FromTextFile <- function(file,
                              start.record.time = NA,
                              end.record.time = NA){
  ## read data from file
  songs <- read.table(file)
  ## sort file by start time
  songs <- songs[with(songs, order(songs[,1])), ]
  ## ensure that all songs have a positive duration
  lengths <- songs[,2] - songs[,1]
  if (sum(lengths <= 0.) > 0){
    print("The file contains songs of null or negative duration!")
    return(-1)
  }
  ## check whether the user entered start and end record.time
  if (is.na(start.record.time)){
    start.record.time <- min(songs[,1:2])
  }
  if (is.na(end.record.time)){
    end.record.time <- max(songs[,1:2])
  }

  ## build the individual lists
  indivs <- list()
  unique.IDs <- unique(songs[,3])
  for (i in 1:length(unique.IDs)){
    my.ID <- as.character(unique.IDs[i])
    indivs[[my.ID]] <- song.BuildIndiv(my.ID,
                                     as.matrix(
                                       songs[songs[,3] == my.ID, 1:2]
                                     ),
                                     start.record.time,
                                     end.record.time)
  }
  return(indivs)
}

#' @title Extract performance statistics from a matrix or data frame.
#'
#' @description
#' \code{song.FromDataObj} builds a list of performance statistics from a
#' matrix or data frame containing the start time, end time, and singer
#' identity for each song in an interaction. This function is an extension of
#' \code{\link{song.BuildIndiv}}.
#'
#' @param songs An n x 3 matrix or data frame containing the start time, end
#' time, and singer identity for each of n songs. The first column contains the
#' start times of each song; the second column contains the end times of each
#' song; the third column specifies the name of the individual that produced
#' the song.
#' @param start.record.time A numeric value indicating the start time of the
#' interaction. An optional parameter - if not specified, the minimum start
#' time is used (i.e. the start of the first song).
#' @param end.record.time A numeric value indicating the end time of the
#' interaction. An optional parameter - if not specified, the maximum end time
#' is used (i.e. the end of the last song).
#'
#' @return
#' \code{song.FromDataObj} returns a list containing the following components
#' for each individual:
#' \describe{
#'   \item{\code{ID}}{A character string giving the name of the individual.}
#'   \item{\code{start.record.time}}{A numeric value indicating the start time
#'   of the interaction.}
#'   \item{\code{end.record.time}}{A numeric value indicating the end time of
#'   the interaction.}
#'   \item{\code{songs}}{A matrix containing the start and end times of
#'   each of the individual's songs.}
#'   \item{\code{songs.num}}{An integer value indicating the number of songs
#'   produced by the individual.}
#'   \item{\code{songs.length}}{A named numeric vector containing the durations
#'   of each song produced by the individual.}
#'   \item{\code{songs.total.length}}{A numeric value indicating the total time
#'   for which the individual was singing (i.e. \code{sum(songs.length)}).}
#'   \item{\code{gaps.num}}{An integer value indicating the number of
#'   inter-song intervals in the individual's performance.}
#'   \item{\code{gaps.length}}{A named numeric vector containing the durations
#'   of each inter-song interval in the individual's performance.}
#'   \item{\code{gaps.total.length}}{A numeric value indicating the total time
#'   for which the individual was silent (i.e. \code{sum(gaps.length)}).}
#' }
#'
#' @examples
#' c <- song.FromDataObj(chickadees)
#'
#' w <- song.FromDataObj(wrens)
#'
#' m <- song.FromDataObj(manakins, 100, 250)
#'
#' @seealso
#' \code{\link{song.FromTextFile}} for data stored in a tab-delimited text
#' file.
#' @export

song.FromDataObj <- function(songs,
                              start.record.time = NA,
                              end.record.time = NA){

  ## sort by start time
  songs <- songs[with(songs, order(songs[,1])), ]
  ## ensure that all songs have a positive duration
  lengths <- songs[,2] - songs[,1]
  if (sum(lengths <= 0.) > 0){
    print("The data contain songs of null or negative duration!")
    return(-1)
  }
  ## check whether the user entered start and end record.time
  if (is.na(start.record.time)){
    start.record.time <- min(songs[,1:2])
  }
  if (is.na(end.record.time)){
    end.record.time <- max(songs[,1:2])
  }

  ## build the individual lists
  indivs <- list()
  unique.IDs <- unique(songs[,3])
  for (i in 1:length(unique.IDs)){
    my.ID <- as.character(unique.IDs[i])
    indivs[[my.ID]] <- song.BuildIndiv(my.ID,
                                     as.matrix(
                                       songs[songs[,3] == my.ID, 1:2]
                                     ),
                                     start.record.time,
                                     end.record.time)
  }
  return(indivs)
}
