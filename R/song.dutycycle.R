#' @title The duty cycle method for calculating chance overlap.
#'
#' @description
#' \code{song.DutyCycleMethod} calculates the expected amount of chance overlap
#' for a given interaction using the duty cycle method (Ficken et al. 1974),
#' then performs a chi-squared test to compare the observed and expected
#' values.
#'
#' @details
#' The duty cycle method was first described by Ficken, Ficken, and Hailman
#' (1974) in their study of acoustic interference in two avian species:
#' red-eyed vireos and least flycatchers. Traditionally, this method returns
#' the expected number of overlapping songs, but can be modified to return the
#' total duration of overlap by specifying \code{\link{song.DutyCycleTime}} as
#' the \code{dc.function}.
#'
#' @param indivs A list created by \code{\link{song.FromDataObj}} or
#' \code{\link{song.FromTextFile}} that contains the performance statistics of
#' each individual.
#'
#' @param dc.function The function to be used to calculate the expected amount
#' of overlap. \code{\link{song.DutyCycleNum}} returns the number of
#' overlapping songs; \code{\link{song.DutyCycleTime}} returns the duration of
#' overlap in seconds. If not specified, \code{song.DutyCycleNum} is used.
#'
#' @return \code{song.DutyCycleMethod} returns a data frame containing the
#' observed amount of overlap, the expected amount of overlap, the value of the
#' chi-squared test statistic, and the resulting p-value for each possible pair
#' of individuals.
#'
#' @examples
#' w <- song.FromDataObj(wrens)
#' ## Traditional method: number of overlapping songs
#' song.DutyCycleMethod(w)
#' ## Modified method: duration of overlap
#' song.DutyCycleMethod(w, song.DutyCycleTime)
#'
#' @references
#' Ficken RW, Ficken MS, Hailman JP. 1974. Temporal pattern shifts to avoid
#' acoustic interference in singing birds. Science. 183:762-763.
#'
#' @import reshape2
#' @export

song.DutyCycleMethod <- function(indivs, dc.function = song.DutyCycleNum){
  num.indivs <- length(indivs)
  indiv.names <- rep("", num.indivs)
  observed <- matrix(0, num.indivs, num.indivs)
  expected <- matrix(0, num.indivs, num.indivs)
  statistic <- matrix(0, num.indivs, num.indivs)
  p.values <- matrix(0, num.indivs, num.indivs)
  for (i in 1:num.indivs){
    indiv.names[i] <- as.character(indivs[[i]]$ID)
    reference <- indivs[[i]]
    for (j in 1:num.indivs){
      indiv.names[j] <- as.character(indivs[[j]]$ID)
      target <- indivs[[j]]
      tmp <- dc.function(target,reference)
      observed[i,j] <- tmp$observed[1]
      expected[i,j] <- tmp$expected[1]
      statistic[i,j] <- unname(tmp$statistic)
      p.values[i,j] <- tmp$p.value
    }
  }
  colnames(observed) <- indiv.names
  rownames(observed) <- indiv.names
  colnames(expected) <- indiv.names
  rownames(expected) <- indiv.names
  colnames(statistic) <- indiv.names
  rownames(statistic) <- indiv.names
  colnames(p.values) <- indiv.names
  rownames(p.values) <- indiv.names

  obs.df <- melt(observed, varnames=c("Reference", "Target"), value.name= "Observed")
  exp.df <- melt(expected, varnames=c("Reference", "Target"))
  stat.df <- melt(statistic, varnames=c("Reference", "Target"))
  p.df <- melt(p.values, varnames=c("Reference", "Target"))
  all.df <- cbind(obs.df, "Expected" = exp.df[,3],"X.squared" = stat.df[,3],
                  "P.value" = p.df[,3])
  output <- all.df[which(all.df$Target != all.df$Reference),]
  return(output)
}

#' @title Predict the number of overlapping songs using the duty cycle method.
#'
#' @description
#' \code{song.DutyCycleNum} calculates the expected number of overlapping and
#' non-overlapping songs, then performs a chi-squared test to compare the
#' observed and expected values. This function is to be used with
#' \code{\link{song.DutyCycleMethod}}.
#'
#' @details
#' The duty cycle method was first described by Ficken, Ficken, and Hailman
#' (1974) in their study of acoustic interference in two avian species:
#' red-eyed vireos and least flycatchers. According to this method, the
#' expected number of flycatcher songs that overlap vireo songs is equal to the
#' total number of flycatcher songs multiplied by the probability that the
#' vireo is singing.
#'
#' @param reference,target Lists created using \code{\link{song.FromDataObj}}
#' or \code{\link{song.FromTextFile}} that contain the performance statistics
#' of two individuals. The amount of overlap is calculated for the target
#' individual with respect to the reference individual.
#'
#' @return \code{song.DutyCycleNum} returns a list with class "\code{htest}"
#' containing the following components:
#' \describe{
#'   \item{\code{statistic}}{the value of the chi-squared test statistic.}
#'   \item{\code{parameter}}{the degrees of freedom of the approximate
#'   chi-squared distribution of the test statistic.}
#'   \item{\code{p.value}}{the p-value for the test.}
#'   \item{\code{method}}{a character string indicating the type of test
#'   performed.}
#'   \item{\code{data.name}}{a character string giving the names of the
#'   individuals included in the analysis.}
#'   \item{\code{observed}}{a numeric vector containing the observed number of
#'   overlapping and non-overlapping songs.}
#'   \item{\code{expected}}{a numeric vector containing the expected number of
#'   overlapping and non-overlapping songs.}
#'   \item{\code{residuals}}{the Pearson residuals, (\code{observed} -
#'   \code{expected}) / sqrt(\code{expected}).}
#' }
#'
#' @examples
#' c <- song.FromDataObj(chickadees)
#' song.DutyCycleNum(c$bird, c$playback)
#'
#' @references
#' Ficken RW, Ficken MS, Hailman JP. 1974. Temporal pattern shifts to avoid
#' acoustic interference in singing birds. Science. 183:762-763.
#'
#' @seealso
#' \code{\link{song.DutyCycleTime}} for calculating the \emph{duration} of
#' overlap rather than the number of overlapping songs.
#'
#' @export

song.DutyCycleNum <- function(target, reference){
  ## calculate proportion of time reference is singing
  reference.total.time <- reference$end.record.time - reference$start.record.time
  p1 <- reference$songs.total.length / reference.total.time
  ## count number of songs of target
  f2 <- target$songs.num

  Expected.numsongs.overlapping <- f2 * p1
  Expected.numsongs.nonoverlapping <- f2 * (1 - p1)
  Observed.numsongs.overlapping <- song.NumOverlap(target$songs, reference$songs)
  Observed.numsongs.nonoverlapping <- target$songs.num - Observed.numsongs.overlapping

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
  DNAME <- paste(target$ID, "and", reference$ID)
  PVAL <- p.value
  METHOD <- "Duty Cycle Method (Ficken et al. 1974)"
  x <- c(Observed.numsongs.overlapping, Observed.numsongs.nonoverlapping)
  E <- c(Expected.numsongs.overlapping, Expected.numsongs.nonoverlapping)
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = x,
                 expected = E, residuals = (x - E)/sqrt(E)), class = "htest")
}


#' @title Predict the duration of overlap using the duty cycle method.
#'
#' @description
#' \code{song.DutyCycleTime} calculates the expected durations of overlap and
#' non-overlap, then performs a chi-squared test to compare the
#' observed and expected values. This function is to be used with
#' \code{\link{song.DutyCycleMethod}}.
#'
#' @details
#' The duty cycle method was first described by Ficken, Ficken, and Hailman
#' (1974) in their study of acoustic interference in two avian species:
#' red-eyed vireos and least flycatchers. This is a modification to their
#' method that calculates the duration of overlap rather than the number of
#' overlapping songs. According to this method, the expected duration for which
#' a flycatcher is overlapping a vireo is equal to the probability that the
#' flycatcher is singing multiplied by the probability that the vireo is
#' singing.
#'
#' @param reference,target Lists created using \code{\link{song.FromDataObj}}
#' or \code{\link{song.FromTextFile}} that contain the performance statistics
#' of two individuals. The amount of overlap is calculated for the target
#' individual with respect to the reference individual.
#'
#' @return \code{song.DutyCycleTime} returns a list with class "\code{htest}"
#' containing the following components:
#' \describe{
#'   \item{\code{statistic}}{the value of the chi-squared test statistic.}
#'   \item{\code{parameter}}{the degrees of freedom of the approximate
#'   chi-squared distribution of the test statistic.}
#'   \item{\code{p.value}}{the p-value for the test.}
#'   \item{\code{method}}{a character string indicating the type of test
#'   performed.}
#'   \item{\code{data.name}}{a character string giving the names of the
#'   individuals included in the analysis.}
#'   \item{\code{observed}}{a numeric vector containing the observed duration
#'   of overlap and non-overlap.}
#'   \item{\code{expected}}{a numeric vector containing the expected duration
#'   of overlap and non-overlap.}
#'   \item{\code{residuals}}{the Pearson residuals, (\code{observed} -
#'   \code{expected}) / sqrt(\code{expected}).}
#' }
#'
#' @examples
#' c <- song.FromDataObj(chickadees)
#' song.DutyCycleTime(c$bird, c$playback)
#'
#' @references
#' Ficken RW, Ficken MS, Hailman JP. 1974. Temporal pattern shifts to avoid
#' acoustic interference in singing birds. Science. 183:762-763.
#'
#' @seealso
#' \code{\link{song.DutyCycleNum}} for calculating the \emph{number} of
#' overlapping songs rather than the duration of overlap.
#'
#' @export

song.DutyCycleTime <- function(target, reference){

  ## calculate proportion of time reference is singing
  total.time <- reference$end.record.time - reference$start.record.time
  p1 <- reference$songs.total.length / total.time

  ## calculate proportion of time target is singing
  p2 <- target$songs.total.length / total.time

  Expected.time.overlapping <- (p2 * p1) * total.time
  Expected.time.nonoverlapping <- p2 * (1 - p1) * total.time
  Observed.time.overlapping <- song.TimeOverlap(target$songs, reference$songs)
  Observed.time.nonoverlapping <- target$songs.total.length - Observed.time.overlapping

  ValueForChiSq <- (Expected.time.overlapping -
                      Observed.time.overlapping)^2 /
    Expected.time.overlapping +
    (Expected.time.nonoverlapping -
       Observed.time.nonoverlapping)^2  /
    Expected.time.nonoverlapping

  p.value <- 1.0 - pchisq(ValueForChiSq, 1)
  ## Build "htest" object (which R uses for statistical tests)
  STATISTIC <- ValueForChiSq
  names(STATISTIC) <- "X-squared"
  PARAMETER <- 1
  names(PARAMETER) <- "df"
  DNAME <- paste(target$ID, "and", reference$ID)
  PVAL <- p.value
  METHOD <- "Duty Cycle Method (modified from Ficken et al. 1974)"
  x <- c(Observed.time.overlapping, Observed.time.nonoverlapping)
  E <- c(Expected.time.overlapping, Expected.time.nonoverlapping)
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = x,
                 expected = E, residuals = (x - E)/sqrt(E)), class = "htest")
}
