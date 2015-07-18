#' @title Summarize the results of a simulation.
#'
#' @description
#' \code{song.Summarize} creates a data frame summarizing the output of
#' \code{\link{song.Simulate}}.
#'
#' @param results A list created by \code{\link{song.Simulate}} that contains
#' the observed and expected amounts of overlap for an interaction.
#'
#' @return \code{song.Summarize} returns a data frame containing the observed
#' amount of overlap, the expected amount of overlap, and the resulting p-value
#' for each possible pair of individuals. Overlap is measured for the target
#' individual with respect to the reference individual.
#'
#' @examples
#' w <- song.BuildAllIndivs(wrens)
#' w.rand <- song.Simulate(w, 100, song.TimeOverlap,
#'                         song.RandomizeKeepSongOrder)
#' song.Summarize(w.rand)
#'
#' @import reshape2
#' @export
song.Summarize <- function(results){
  ## extract results
  obs <- results$observed
  exp <- results$expected
  p.values <- results$p.values
  obs.df <- melt(obs, varnames=c("Reference", "Target"),
                 value.name= "Observed")
  exp.df <- melt(exp)
  pval.df <- melt(p.values)
  ## create results data frame
  results.df <- cbind(obs.df, "Expected"= exp.df[,3],
                      "P-value" = pval.df[,3])
  output <- results.df[which(results.df$Reference != results.df$Target),]
  return(output)
}
