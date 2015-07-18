#' @title Compare observed overlap to chance expectations.
#'
#' @description
#' \code{song.Simulate} calculates the expected amount of chance overlap for a
#' given interaction, then compares the observed amount of overlap to
#' this expectation to generate a p-value.
#'
#' @details
#' \code{song.Simulate} generates a user-defined number of randomized
#' performances for each individual. The function then selects one individual
#' as the "reference" and another as the "target" to create a set of randomized
#' interactions, pairing the reference individual's observed performance with
#' each of the target's randomized performances. \code{song.Simulate}
#' calculates the amount of overlap occuring during each of these randomized
#' interactions to generate the null distribution (i.e. the amount of overlap
#' expected due to chance). The p-value is calculated as the percentage of
#' randomized interactions in which the amount of overlap is greater than the
#' amount of overlap in the observed interaction. This process is repeated for
#' each possible combination of reference and target individuals.
#'
#' @param indivs A list created by \code{\link{song.BuildAllIndivs}} or
#' \code{\link{song.ReadSongList}} that contains the performance statistics of
#' each individual.
#' @param num.rand A numeric value indicating the desired number of
#' randomizations.
#' @param overlap.function The function to be used to calculate the amount of
#' overlap. \code{\link{song.TimeOverlap}} returns the duration of overlap
#' in seconds; \code{\link{song.NumOverlap}} returns the number of overlapping
#' songs.
#' @param randomize.function The function to be used to generate the randomized
#' performances for each individual. Options include
#' \code{\link{song.RandomizeSampleGaps}},
#' \code{\link{song.RandomizeKeepGaps}}, and
#' \code{\link{song.RandomizeKeepSongOrder}}.
#'
#' @return \code{song.Simulate} returns a list containing the components
#' described below. In each output matrix, the row name specifies the reference
#' individual, while the column name specifies the target (or randomized)
#' individual. Overlap is measured for the target individual with respect to
#' the reference individual.
#' \describe{
#'   \item{observed}{A matrix containing the amount of overlap in the observed
#'   interaction for each possible pair of individuals.}
#'   \item{expected}{A matrix containing the average amount of overlap in the
#'   randomized interactions for each possible pair of individuals. These
#'   values represent the expected amounts of overlap due to chance.}
#'   \item{p.values}{A matrix containing the p-values associated with each
#'   possible pair of individuals.}
#'   \item{randomized}{A three-dimensional array containing the amount of
#'   overlap occurring in each randomized interaction. Each "slice" corresponds
#'   to one randomization, containing the amount of overlap for each possible pair of
#'   individuals. These values make up the null distribution.}
#'   \item{overlap.method}{The function used to calculate the amount of
#'   overlap.}
#'   \item{randomize.method}{The function used to generate the randomized
#'   interactions.}
#' }
#'
#' @examples
#' c <- song.BuildAllIndivs(chickadees)
#' c.sim <- song.Simulate(c, num.rand = 100, song.TimeOverlap,
#'                         song.RandomizeSampleGaps)
#' ## Duration of overlap in the observed interaction
#' c.sim$observed
#' ## Duration of overlap expected due to chance
#' c.sim$expected
#' ## How does observed overlap compare to chance?
#' c.sim$p.values
#'
#' @seealso
#' \code{\link{song.PlotResultsDensity}} to visualize the output of
#' \code{song.Simulate}.
#'
#' @export

song.Simulate <- function(indivs,
                          num.rand = 100,
                          overlap.function = "song.TimeOverlap",
                          randomize.function = "song.RandomizeSampleGaps"){
  ptm <- proc.time()
  ## match the functions
  f.overlap <- match.fun(overlap.function)
  f.randomize <- match.fun(randomize.function)
  ## randomize songs
  num.indivs <- length(indivs)
  for (i in 1:num.indivs){
    b <- indivs[[i]]
    print(paste("Randomizing", b$ID))
    songs.num <- b$songs.num
    b$random.songs <- array(0, c(songs.num, 2, num.rand))
    for (j in 1:num.rand){
      b$random.songs[,,j] <- f.randomize(b)
    }
    indivs[[i]] <- b
  }
  ## now compute the overlaps, i = reference, j = target
  observed <- matrix(0, num.indivs, num.indivs)
  expected <- matrix(0, num.indivs, num.indivs)
  randomized <- array(0, c(num.indivs, num.indivs, num.rand))
  p.values <- matrix(0, num.indivs, num.indivs)
  indiv.names <- rep("", num.indivs)
  for (i in 1:num.indivs){
    indiv.names[i] <- as.character(indivs[[i]]$ID)
    print(paste("Running simulations for", indiv.names[i]))
    for (j in 1:num.indivs){
      indiv.names[j] <- as.character(indivs[[j]]$ID)
      print(paste("... and", indiv.names[j]))
      observed[i, j] <- f.overlap(indivs[[j]]$songs, indivs[[i]]$songs)
      randomized[i, j, ] <- apply(indivs[[j]]$random.songs, 3,
                                  f.overlap, indivs[[i]]$songs)
      expected[i,j] <- sum(randomized[i, j, ] )
      ## sum of cases in which randomized overlap is higher than observed
      p.values[i,j] <- sum(randomized[i,j,] >= observed[i,j])
    }
  }
  ## normalize to obtain p.values
  p.values <- p.values / num.rand
  ## get means
  expected <- expected / num.rand
  colnames(p.values) <- indiv.names
  rownames(p.values) <- indiv.names
  colnames(observed) <- indiv.names
  rownames(observed) <- indiv.names
  colnames(expected) <- indiv.names
  rownames(expected) <- indiv.names
  print(proc.time() - ptm)
  return(list(observed = observed,
              expected = expected,
              p.values = p.values,
              randomized = randomized,
              overlap.method = overlap.function,
              randomize.method = randomize.function))
}
