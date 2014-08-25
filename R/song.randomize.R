#################################################
## FUNCTIONS FOR RANDOMIZATION (NULL MODELS) ####
#################################################
song.RandomizeSampleGaps <- function(bird){
  ## in this randomization, the length of the songs
  ## is kept, but the order of the songs is randomized.
  ## Also, the gap lengths are sampled at random
  ## such that the total gap length is kept constant.

  rnd.recital <- bird$recital
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
  rnd.recital[,1] <- start.songs
  rnd.recital[,2] <- end.songs
  return(rnd.recital)
}

song.RandomizeKeepGaps <- function(bird){
  ## randomize song order and gaps order

  rnd.recital <- bird$recital
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
  rnd.recital[,1] <- start.songs
  rnd.recital[,2] <- end.songs
  return(rnd.recital)
}

song.RandomizeKeepSongOrder <- function(bird){
  ## randomize gaps order but keep the songs
  ## in the same order as the observed recital of bird

  rnd.recital <- bird$recital
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
  rnd.recital[,1] <- start.songs
  rnd.recital[,2] <- end.songs
  return(rnd.recital)
}
