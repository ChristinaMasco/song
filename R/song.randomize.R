#################################################
## FUNCTIONS FOR RANDOMIZATION (NULL MODELS) ####
#################################################
song.RandomizeSampleGaps <- function(bird){
  ## in this randomization, the length of the songs
  ## is kept, but the order of the songs is randomized.
  ## Also, the gap lengths are sampled at random
  ## such that the total gap length is kept constant.

  rnd.songs <- bird$songs
  ## randomize the length of the gaps
  rnd.songs.length <- sample(bird$songs.length)
  ## draw random breakpoints for the gaps
  break.points <- c(0,
                    sort(runif(bird$songs.num)) * bird$gaps.total.length,
                    bird$gaps.total.length)
  ## compute gap lengths
  rnd.gaps.length <- break.points[2:length(break.points)] -
    break.points[1:(length(break.points) - 1)]
  ## cumulate the gap and the song lengths
  cumul.gaps <- cumsum(rnd.gaps.length[-length(rnd.gaps.length)])
  cumul.songs <- cumsum(rnd.songs.length)
  ## their sum is the time at which the songs end
  ## (note the offset of bird$start.record.time)
  end.songs <- cumul.gaps + cumul.songs + bird$start.record.time
  ## the start time can be found by difference
  start.songs <- end.songs - rnd.songs.length
  rnd.songs[,1] <- start.songs
  rnd.songs[,2] <- end.songs
  return(rnd.songs)
}

song.RandomizeKeepGaps <- function(bird){
  ## randomize song order and gaps order

  rnd.songs <- bird$songs
  rnd.songs.length <- sample(bird$songs.length)
  rnd.gaps.length <- sample(bird$gaps.length)
  ## cumulate the gap and the song lengths
  cumul.gaps <- cumsum(rnd.gaps.length[-length(rnd.gaps.length)])
  cumul.songs <- cumsum(rnd.songs.length)
  ## their sum is the time at which the songs end
  ## (note the offset of bird$start.record.time)
  end.songs <- cumul.gaps + cumul.songs + bird$start.record.time
  ## the start time can be found by difference
  start.songs <- end.songs - rnd.songs.length
  rnd.songs[,1] <- start.songs
  rnd.songs[,2] <- end.songs
  return(rnd.songs)
}

song.RandomizeKeepSongOrder <- function(bird){
  ## randomize gaps order but keep the songs
  ## in the same order as the observed songs of bird

  rnd.songs <- bird$songs
  rnd.songs.length <- bird$songs.length
  rnd.gaps.length <- sample(bird$gaps.length)
  ## cumulate the gap and the song lengths
  cumul.gaps <- cumsum(rnd.gaps.length[-length(rnd.gaps.length)])
  cumul.songs <- cumsum(rnd.songs.length)
  ## their sum is the time at which the songs end
  ## (note the offset of bird$start.record.time)
  end.songs <- cumul.gaps + cumul.songs + bird$start.record.time
  ## the start time can be found by difference
  start.songs <- end.songs - rnd.songs.length
  rnd.songs[,1] <- start.songs
  rnd.songs[,2] <- end.songs
  return(rnd.songs)
}
