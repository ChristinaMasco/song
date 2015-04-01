#' Perform duty-cycle test ()
#' @param bird2
#' @param bird1
#'
song.DutyCycle <- function(bird2, bird1){
  ## statistics on bird1
  ## Total time bird1 sings
  bird1.total.time <- bird1$end.record.time - bird1$start.record.time
  ## proportion of time bird1 is singing
  p1 <- bird1$songs.total.length / bird1.total.time
  ## number of songs of bird2
  f2 <- bird2$songs.num

  Expected.numsongs.overlapping <- f2 * p1
  Expected.numsongs.nonoverlapping <- f2 * (1 - p1)
  Observed.numsongs.overlapping <- song.NumOverlap(bird1$songs, bird2$songs)
  Observed.numsongs.nonoverlapping <- bird2$songs.num - Observed.numsongs.overlapping

  ValueForChiSq <- (Expected.numsongs.overlapping -
                      Observed.numsongs.overlapping)^2 /
    Expected.numsongs.overlapping +
    (Expected.numsongs.nonoverlapping -
       Observed.numsongs.nonoverlapping)^2  /
    Expected.numsongs.nonoverlapping

  p.value <- 1.0 - pchisq(ValueForChiSq, 1)
  ## Build "htest" object (which R uses for statistical tests)
  STATISTIC <- ValueForChiSq
  names(STATISTIC) <- "X-squared"
  PARAMETER <- 1
  names(PARAMETER) <- "df"
  DNAME <- paste("bird", bird1$ID, "and bird", bird2$ID)
  PVAL <- p.value
  METHOD <- "Duty Cycle (Ficken et al., 1974)"
  x <- c(Observed.numsongs.overlapping, Observed.numsongs.nonoverlapping)
  E <- c(Expected.numsongs.overlapping, Expected.numsongs.nonoverlapping)
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = x,
                 expected = E, residuals = (x - E)/sqrt(E)), class = "htest")
}
