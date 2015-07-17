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