#' @title The duty cycle method for calculating chance overlap.
#' 
#' @description
#' \code{song.DutyCycle} calculates the expected number of overlapping and 
#' non-overlapping songs, then performs a chi-squared test to compare the 
#' observed and expected values.  
#'
#' @details
#' The duty cycle method was first described by Ficken, Ficken, and Hailman 
#' (1974) in their study of acoustic interference in two avian species: 
#' red-eyed vireos and least flycatchers.
#'
#' @param indiv1,indiv2 Lists created using \code{song.BuildSongList} or 
#' \code{song.ReadSongList} that contain the performances of individuals 1 and
#' 2 respectively.
#'
#' @return \code{song.DutyCycle} returns a list with class "\code{htest}" 
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
#' c <- song.BuildSongList(chickadees)
#' song.DutyCycle(c$bird, c$playback)
#' 
#' w <- song.BuildSongList(wrens)
#' song.DutyCycle(w$male, w$female)
#' 
#' m <- song.BuildSongList(manakins)
#' song.DutyCycle(m$pairA, m$pairB)
#'
#' @references
#' Ficken, R.W., Ficken, M.S. & Hailman, J.P. (1974). Temporal pattern shifts to
#' avoid acoustic interference in singing birds. \emph{Science}, \strong{183}, 
#' 762-763. 
#'
song.DutyCycle <- function(indiv2, indiv1){
  ## calculate proportion of time indiv1 is singing
  indiv1.total.time <- indiv1$end.record.time - indiv1$start.record.time
  p1 <- indiv1$songs.total.length / indiv1.total.time
  ## count number of songs of indiv2
  f2 <- indiv2$songs.num

  Expected.numsongs.overlapping <- f2 * p1
  Expected.numsongs.nonoverlapping <- f2 * (1 - p1)
  Observed.numsongs.overlapping <- song.NumOverlap(indiv1$songs, indiv2$songs)
  Observed.numsongs.nonoverlapping <- indiv2$songs.num - Observed.numsongs.overlapping

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
  DNAME <- paste(indiv2$ID, "and", indiv1$ID)
  PVAL <- p.value
  METHOD <- "Duty Cycle Method (Ficken et al. 1974)"
  x <- c(Observed.numsongs.overlapping, Observed.numsongs.nonoverlapping)
  E <- c(Expected.numsongs.overlapping, Expected.numsongs.nonoverlapping)
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = x,
                 expected = E, residuals = (x - E)/sqrt(E)), class = "htest")
}
