song.Simulate <- function(birds,
                          num.rand = 250,
                          overlap.function = "song.TimeOverlap",
                          randomize.function = "song.RandomizeSampleGaps"){
  ptm <- proc.time()
  ## match the functions
  f.overlap <- match.fun(overlap.function)
  f.randomize <- match.fun(randomize.function)
  ## randomize birds recitals
  num.birds <- length(birds)
  for (i in 1:num.birds){
    b <- birds[[i]]
    print(paste("Randomizing bird", b$ID))
    songs.num <- b$songs.num
    b$random.recitals <- array(0, c(songs.num, 2, num.rand))
    for (j in 1:num.rand){
      b$random.recitals[,,j] <- f.randomize(b)
    }
    birds[[i]] <- b
  }
  ## now compute the overlaps
  observed <- matrix(0, num.birds, num.birds)
  expected <- matrix(0, num.birds, num.birds)
  randomized <- array(0, c(num.birds, num.birds, num.rand))
  p.values <- matrix(0, num.birds, num.birds)
  bird.names <- rep("", num.birds)
  for (i in 1:num.birds){
    bird.names[i] <- as.character(birds[[i]]$ID)
    print(paste("Running simulations for", bird.names[i]))
    for (j in 1:num.birds){
      bird.names[j] <- as.character(birds[[j]]$ID)
      print(paste("... and", bird.names[j]))
      observed[i, j] <- f.overlap(birds[[j]]$recital, birds[[i]]$recital)
      randomized[i, j, ] <- apply(birds[[j]]$random.recitals, 3,
                                  f.overlap, birds[[i]]$recital)
      expected[i,j] <- sum(randomized[i, j, ] )
      ## sum of cases in which randomized overlap is higher than observed
      p.values[i,j] <- sum(randomized[i,j,] >= observed[i,j])
    }
  }
  ## normalize to obtain p.values
  p.values <- p.values / num.rand
  ## get means
  expected <- expected / num.rand
  colnames(p.values) <- bird.names
  rownames(p.values) <- bird.names
  colnames(observed) <- bird.names
  rownames(observed) <- bird.names
  colnames(expected) <- bird.names
  rownames(expected) <- bird.names
  print(proc.time() - ptm)
  return(list(observed = observed,
              expected = expected,
              p.values = p.values,
              randomized = randomized,
              overlap.method = overlap.function,
              randomize.method = randomize.function))
}
