song.Digitize <- function(recital.1, recital.2, number.points){
  ## minimum time
  mintime <- min(c(recital.1[,1], recital.2[,1]))
  ## maximum time
  maxtime <- max(c(recital.1[,2], recital.2[,2]))
  ## Subdivide the time in number.points
  digital.time <- seq(mintime, maxtime, length.out = number.points)
  digital.recital1 <- rep(0, number.points)
  digital.recital2 <- rep(0, number.points)
  for (i in 1:length(digital.time)){
      t <- digital.time[i]
      digital.recital1[i] <- (sum((t >= recital.1[,1]) & (t < recital.1[,2])) > 0) * 1
      digital.recital2[i] <- (sum((t >= recital.2[,1]) & (t < recital.2[,2])) > 0) * 1
  }
  return(rbind(digital.recital1, digital.recital2))
}

song.PhaseProfile <- function(recital.1, recital.2, number.points = 10000){
  digital.recital <- song.Digitize(recital.1, recital.2, number.points)
  ## now for each possible shift, compute how many points are overlapping
  phase.profile <- matrix(0, number.points, 2)
  phase.profile[,1] <- 0:(number.points - 1)
  for (i in 0:(number.points-1)){
    phase.profile[i + 1, 2] <- sum(digital.recital[1,] *  c(digital.recital[2,][(i + 1):number.points], digital.recital[2,][0:i]))
  }
  #plot(phase.profile[,1]~phase.profile[,2], type = "b")
  return(phase.profile)
}
