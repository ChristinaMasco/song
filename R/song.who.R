# Checks whether two songs overlap and returns the direction of overlap
song.WhoSongsOverlap <- function(my.song1, my.song2){
  ## if songs do not overlap, return 0
  overlapping <- 0
  ## if songs overlap and song2 is lagging, return 1
  if ((my.song1[1] <= my.song2[1]) &&
        (my.song1[2] >= my.song2[1])){
    overlapping <- 1
  }
  ## if songs overlap and song1 is lagging, return -1
  if ((my.song2[1] <= my.song1[1]) &&
        (my.song2[2] >= my.song1[1])) {
    overlapping <- -1
  }
  return(overlapping)
}

# Calculates the number of times a song overlaps all songs in a given recital, direction indicated by sign
song.WhoOneSongWithRecital <- function(my.song, my.recital){
  overlaps <- apply(my.recital, 1, song.WhoSongsOverlap, my.song)
}

# Calculates the number of songs in recital 2 that lag behind and overlap songs in recital 1
song.WhoNumOverlap <- function(recital.2, recital.1){
  overlaps <- apply(recital.1, 1, song.WhoOneSongWithRecital, recital.2)
  r2.overlaps = abs(sum(overlaps < 0))
  return (r2.overlaps)
}

# Calculates the number of times each bird overlaps each other bird's songs
song.WhoOverlaps <- function(birds){
  id <- names(birds)
  overlaps <- data.frame(overlapper=character(), reference=character(), num.overlap=numeric())
  for(i in id){
    for(j in id){
      num <- song.WhoNumOverlap(birds[[i]]$recital, birds[[j]]$recital)
      overlaps <- rbind(overlaps, data.frame(overlapper = i, 
                                             reference = j, num.overlap = num))
    }    
  }
  overlaps <- overlaps[which(overlaps$overlapper != overlaps$reference),]
  return(overlaps)
}