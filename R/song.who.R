# Checks whether two songs overlap and returns the direction of overlap
song.WhichSongsOverlap <- function(my.song1, my.song2){
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

# Calculates the number of times a song overlaps all songs in a given songlist, direction indicated by sign
song.WhichSongWithSongList <- function(my.song, my.songlist){
  overlaps <- apply(my.songlist, 1, song.WhichSongsOverlap, my.song)
}

# Counts the number of songs in songlist 2 that lag behind and overlap songs in songlist 1
song.CountNumOverlap <- function(songlist.2, songlist.1){
  overlaps <- apply(songlist.1, 1, song.WhichSongWithSongList, songlist.2)
  s2.overlaps = abs(sum(overlaps < 0))
  return (s2.overlaps)
}

# Calculates the number of times each bird overlaps each other bird's songs
song.WhoOverlaps <- function(birds){
  id <- names(birds)
  overlaps <- data.frame(overlapper=character(), reference=character(), num.overlap=numeric())
  for(i in id){
    for(j in id){
      num <- song.CountNumOverlap(birds[[i]]$songs, birds[[j]]$songs)
      overlaps <- rbind(overlaps, data.frame(overlapper = i, 
                                             reference = j, num.overlap = num))
    }    
  }
  overlaps <- overlaps[which(overlaps$overlapper != overlaps$reference),]
  return(overlaps)
}