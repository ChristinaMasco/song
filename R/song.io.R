song.BuildBird <- function(ID, songs, start.record.time, end.record.time){
  ## create a bird:
  ## a list that includes some stats on the songs
  bird <- list()
  ## bird identifier
  bird$ID <- ID
  ## info on the recording time
  bird$start.record.time <- start.record.time
  bird$end.record.time <- end.record.time
  ## songs [start, stop] for each song
  colnames(songs) <- c("start", "end")
  rownames(songs) <- 1:dim(songs)[1]
  bird$songs <- songs
  ## number of songs
  bird$songs.num <- dim(songs)[1]
  ## length of the songs
  bird$songs.length <- songs[,2] - songs[,1]
  ## total time sang
  bird$songs.total.length <- sum(bird$songs.length)
  ## gaps in between songs
  for.gaps.start <- c(songs[,1], end.record.time)
  for.gaps.end <- c(start.record.time, songs[,2])
  bird$gaps.num <- bird$songs.num + 1
  bird$gaps.length <- for.gaps.start - for.gaps.end
  bird$gaps.total.length <- sum(bird$gaps.length)
  ## check: gaps must be of positive length
  if (sum(bird$gaps.length < 0.) > 0){
    print(paste(bird$ID, "has inter-song intervals of negative length!"))
    ## print rows with negative gap lengths
    start <- as.vector(bird$songs[,1])
    end <- as.vector(bird$songs[,2])
    interval <- head(as.vector(bird$gaps.length),-1)
    output <- data.frame(start,end,interval)
    print(output[which(output$interval<0),], row.names=TRUE)
    return(output)
  }  
  return(bird)
}

#' Reads a file containing the songs for the birds
#'
#' @param file.songs A file containing the start time and end time of all the songs
#' @param start.record.time Optional parameter to set a particular start time for the recording
#' @param end.record.time Optional parameter to set a particular end time for the recording
#' @examples
#' \dontrun{
#' Rec <- song.ReadSongList("test.txt", 0.1, 100)
#' }
#'@return A list of birds along with their songs
song.ReadSongList <- function(file.songs,
                              start.record.time = NA,
                              end.record.time = NA){
  ## read a list of songs and build
  ## the bird lists

  ## The input file is a space-separated file
  ## whose rows are startsong endsong birdID

  ## one can specify the start record time
  ## and the end record time.
  ## if they are not specified, the min and max
  ## are taken (respectively)

  songs <- read.table(file.songs)
  #sort file by start time
  songs <- songs[with(songs, order(songs[,1])), ]
  ## sanity check: each song must be of positive length
  lengths <- songs[,2] - songs[,1]
  if (sum(lengths <= 0.) > 0){
    print("The file contains songs of null or negative length!")
    return(-1)
  }
  ## check whether the user entered start and end record.time
  if (is.na(start.record.time)){
    start.record.time <- min(songs[,1:2])
  }
  if (is.na(end.record.time)){
    end.record.time <- max(songs[,1:2])
  }

  ## proceed building the birds
  birds <- list()
  unique.IDs <- unique(songs[,3])
  for (i in 1:length(unique.IDs)){
    my.ID <- as.character(unique.IDs[i])
    birds[[my.ID]] <- song.BuildBird(my.ID,
                                     as.matrix(
                                       songs[songs[,3] == my.ID, 1:2]
                                     ),
                                     start.record.time,
                                     end.record.time)
  }
  return(birds)
}

song.BuildSongList <- function(songs,
                              start.record.time = NA,
                              end.record.time = NA){

  #sort by start time
  songs <- songs[with(songs, order(songs[,1])), ]
  ## sanity check: each song must be of positive length
  lengths <- songs[,2] - songs[,1]
  if (sum(lengths <= 0.) > 0){
    print("The file contains songs of null or negative length!")
    return(-1)
  }
  ## check whether the user entered start and end record.time
  if (is.na(start.record.time)){
    start.record.time <- min(songs[,1:2])
  }
  if (is.na(end.record.time)){
    end.record.time <- max(songs[,1:2])
  }

  ## proceed building the birds
  birds <- list()
  unique.IDs <- unique(songs[,3])
  for (i in 1:length(unique.IDs)){
    my.ID <- as.character(unique.IDs[i])
    birds[[my.ID]] <- song.BuildBird(my.ID,
                                     as.matrix(
                                       songs[songs[,3] == my.ID, 1:2]
                                     ),
                                     start.record.time,
                                     end.record.time)
  }
  return(birds)
}

song.BatchReadSongs <- function(filelist){
  birdlist <- list()
  for (i in 1:length(filelist)){
    birdlist[[i]] <- song.ReadSongList(filelist[i])
  }
  return(birdlist)
}