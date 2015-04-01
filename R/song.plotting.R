song.PlotSongs <- function(birds){
  all.songs <- matrix(0, 0, 2)
  all.names <- character(0)
  for (i in 1:length(birds)){
    all.songs <- rbind(all.songs, birds[[i]]$songs)
    all.names <- c(all.names, rep(birds[[i]]$ID, birds[[i]]$songs.num))
  }
  my.df <- data.frame(Birds = all.names,
                      Start = all.songs[,1],
                      End = all.songs[,2])
  songs.plot <- ggplot(data = my.df, aes(x = Birds,
                                           ymin = Start, ymax = End, colour = Birds)) +
    geom_linerange(size = I(2)) + scale_y_continuous("Time") +
    coord_flip() + theme_bw() +
    theme(legend.position = "none")
  return(songs.plot)
}

song.PlotResultsDensity <- function(results){
  observed.df <- melt(results$observed)
  expected.df <- melt(results$expected)
  p.values.df <- melt(results$p.values)
  all.df <- observed.df
  all.df <- cbind(all.df, expected.df[,3])
  all.df <- cbind(all.df, p.values.df[,3])
  names(all.df) <- c("Target", "Randomized", "Observed", "Expected", "p.value")
  density.df <- melt(results$randomized,
                     names = c("Target", "Randomized", "randnum"))[,-3]
  names(density.df) <- c("Target", "Randomized", "Overlap")
  birds.names <- colnames(results$observed)
  density.df$Target <- as.factor(birds.names[density.df$Target])
  ## make the order of the rows the same as in the p.values table
  density.df$Target <- factor(density.df$Target, colnames(results$p.values))
  density.df$Randomized <- as.factor(birds.names[density.df$Randomized])
  ## make the order of the cols the same as in the p.values table
  density.df$Randomized <- factor(density.df$Randomized, colnames(results$p.values))

  ## code for p-values labels
  bird.names <- colnames(results$observed)
  tmp.targets <- character(0)
  tmp.randomized <- character(0)
  tmp.maxdensity <- numeric(0)
  tmp.pvalue <- character(0)
  tmp.xvalue <- numeric(0)

  for (T in bird.names){
    for (R in bird.names){
      x <- density.df$Overlap[density.df$Target == T & density.df$Randomized == R]
      maxdens <- max(density(x, kernel = "gaussian", adjust = 1)$y)
      tmp.targets <- c(tmp.targets, T)
      tmp.randomized <- c(tmp.randomized, R)
      tmp.maxdensity <- c(tmp.maxdensity, maxdens)
      mypv <- all.df$p.value[all.df$Target == T & all.df$Randomized == R]
      if (mypv < 0.01){
        tmp.pvalue <- c(tmp.pvalue,
                        "p<0.01")
      } else{
        tmp.pvalue <- c(tmp.pvalue,
                        paste("p=", round(mypv,2), sep =""))
      }
      tmp.xvalue <- c(tmp.xvalue, max(results$observed))
    }
  }
  p.df <- data.frame(Target = tmp.targets, Randomized = tmp.randomized, xpos = tmp.xvalue, ypos = tmp.maxdensity, mylabel = tmp.pvalue)
  ## adjust vertical alignement
  for (T in bird.names){
    p.df$ypos[p.df$Target == T] <- max( p.df$ypos[p.df$Target == T])
  }

  overlap.density.plot <- ggplot(data = density.df,
                                 aes(x = Overlap, fill = Target, colour = Randomized)) +
    geom_density(alpha = 0.2) +  theme_bw() +
    geom_vline(data = all.df,
               aes(xintercept = Observed),
               colour = I("black"), linetype = 2, alpha = 0.5, size = 0.75) +                                                    geom_text(data = p.df, aes(x = xpos, y = ypos, label = mylabel), hjust = 1, vjust = 1, size = 4, col = "black") +
    facet_grid(Target ~ Randomized, scales = "free_y") +
    theme(legend.position = "none")
  return(overlap.density.plot)
}
