song.BatchReadSongs <- function(filelist){
  indivlist <- lapply(filelist, song.ReadSongList)
  return(indivlist)
}

song.BatchSimulate <- function(indivlist, num.rand = 100,
                               overlap.function = "song.TimeOverlap",
                               randomize.function = 
                                 "song.RandomizeSampleGaps"){
  output <- lapply(indivlist, song.Simulate, num.rand=num.rand, 
                          overlap.function=overlap.function,
                          randomize.function=randomize.function)
  return(output)
}