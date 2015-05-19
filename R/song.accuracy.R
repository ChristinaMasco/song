
song.TestAccuracy <- function(indivlist, num.rand, overlap.function, 
                              randomize.function){
  ## calculate the observed and expected amounts of overlap
  rndm <- song.BatchSimulate(indivlist, num.rand, overlap.function,
                             randomize.function)
  ## extract values from the results list
  ## setting the first individual to sing as the reference individual
  observed <- unlist(lapply(rndm, function(x) x$observed[2,1]))
  expected <- unlist(lapply(rndm, function(x) x$expected[2,1]))
  p.values <- unlist(lapply(rndm, function(x) x$p.values[2,1]))
  ## count totals for % error calculation  
  if(identical(overlap.function,song.TimeOverlap)){
    ## if TimeOverlap, total = total duration of interaction
    total <- unlist(lapply(indivlist, function(x) x[[1]]$end.record.time -
                             x[[1]]$start.record.time))
  } else{
    ## if NumOverlap, total = total number of songs 
    total <- unlist(lapply(indivlist, function(x) x[[1]]$songs.num + 
                             x[[2]]$songs.num))
  }
  ## error calculations
  dx <- abs(observed - expected)
  avg.dx <- mean(dx)
  avg.pct <- mean(dx/total)
  var.p <- var(p.values)
  num.overlap <- sum(p.values < 0.05)
  num.avoid <- sum(p.values > 0.95)
  ## build output
  output <- c(avg.dx, avg.pct, var.p, num.overlap, num.avoid)
  names(output) <- c("Average Error", "Average Error (%)", "Variance of P",
                     "P < 0.05", "P > 0.95")
  return(output)
}

##Evaluate accuracy as a function of the number of randomizations
## n is a numeric vector specifying the number of randomizations 

song.HowManyRndms <- function(indivlist, n, overlap.function, 
                              randomize.function){
  accuracy <- list()
  runtime <- numeric()
  for (i in 1:length(n)){
    ## start the clock
    ptm <- proc.time()
    ## run the simulation and test accuracy
    accuracy[[i]] <- song.TestAccuracy(indivlist, n[i], overlap.function,
                                       randomize.function)
    ## stop the clock and store elapsed time
    run <- as.vector(proc.time() - ptm)
    runtime[i] <- run[3]    
  }
  ## build output
  results <- do.call(rbind, accuracy)
  output <- data.frame(n=n, avg.error=results[,1], avg.pct=results[,2],
                       var.p=results[,3], runtime=runtime)
  names(output) <- c("N","Average Error", "Average Error (%)", 
                     "Variance of P", "Run Time")
  return(output)
}