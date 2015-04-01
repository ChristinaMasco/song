song.CountTotals <- function(birdlist){
  total.songs <- numeric()
  total.time <- numeric()
  for (i in 1:length(birdlist)){
    total.songs[i] <- birdlist[[i]][[1]]$songs.num + birdlist[[i]][[2]]$songs.num
    total.time[i] <- birdlist[[i]][[1]]$end.record.time - birdlist[[i]][[1]]$start.record.time 
  }
  total <- data.frame(time=total.time, number=total.songs)
  return(total)
}


song.TestAccuracy <- function(birdlist, model, overlap.function){
  dx <- numeric()
  pct <- numeric()
  p <-numeric()
  
  if(identical(overlap.function,song.TimeOverlap)){
    total <- song.CountTotals(birdlist)[,1]
  } else{
    total <- song.CountTotals(birdlist)[,2]
  }
  
  for (i in 1:length(model)){
    mean <- model[[i]]$expected[2,1]
    observed <- model[[i]]$observed[2,1]
    
    num.birds = length(dimnames(model[[i]]$observed)[[1]])
    num.rand = length(model[[i]]$randomized)/num.birds^2
    
    dx[i] <- abs(observed - mean)
    p[i] <- model[[i]]$p.values[2,1]
  }
  
  avg.dx <- mean(dx)
  avg.pct <- mean(dx/total)
  avg.p <- mean(p)
  num.overlap <- sum(p < 0.05)
  num.avoid <- sum(p > 0.95)
  
  output <- c(avg.dx,avg.pct,avg.p,num.overlap,num.avoid)
  names(output) <- c("avg dev", "avg dev (%)", "avg P-value", "P < 0.05", "P > 0.95")
  
  return(output)
  
}

##Evaluate accuracy as a function of the number of randomizations
## n is a numeric vector specifying the number of randomizations 

song.HowManyRandomizations <- function (birdlist, n, overlap.function, randomize.function){
  tests <- data.frame(avg.dx=numeric(), avg.pct=numeric(), avg.p=numeric(), 
                      num.overlap=numeric(), num.avoid=numeric())
  runtime = numeric()
    
  for (i in 1:length(n)){
    # start the clock
    ptm <- proc.time()
    # run the simulation
    model <- song.BatchSimulate(birdlist, n[i], overlap.function, randomize.function)
    # stop the clock and store the elapsed time
    run <- as.vector(proc.time() - ptm)
    runtime[i] <- run[3]
    # test the accuracy of the model
    tests[i,] <- song.TestAccuracy(birdlist,model,overlap.function)
  }
  # return a data frame containing the number of randomizations, the average % deviation, and the run time
  output <- data.frame(n=n, dx=tests[,1], pct=tests[,2], p=tests[,3], runtime=runtime)
  return(output)
}