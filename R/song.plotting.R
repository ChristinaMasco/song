#' @title Plot the relative timing of songs during an interaction.
#'
#' @description
#' \code{song.PlotSongs} creates a \code{\link{ggplot}} object depicting the
#' relative timing of songs during an interaction. Each song is represented by
#' a line segment plotted on a time axis. Songs are color-coded and
#' plotted by individual.
#'
#' @details
#' The optional parameters \code{start.time} and \code{end.time} set the limits
#' of the x-axis of the plot. If specified, these parameters can be used to
#' view a select portion of the interaction. \code{breakpt} wraps the x-axis,
#' displaying the interaction over a series of panels arranged vertically. This
#' feature can be especially useful for viewing short songs over long durations.
#'
#' @param indivs A list created by \code{\link{song.FromDataObj}} or
#' \code{\link{song.FromTextFile}} that contains the performance statistics of
#' each individual.
#' @param start.time A numeric value indicating the start time of the
#' interaction in seconds. An optional parameter - if not specified, the
#' minimum start time is used (i.e. the start of the first song).
#' @param end.time A numeric value indicating the end time of the
#' interaction in seconds. An optional parameter - if not specified, the
#' maximum end time is used (i.e. the end of the last song).
#' @param breakpt An optional parameter for wrapping the time axis using the
#' function \code{\link{facet_grid}}. \code{breakpt} is a numeric value
#' indicating the duration of each plot panel.
#' @param line.wt A numeric value indicating the line thickness.
#'
#' @examples
#' c <- song.FromDataObj(chickadees)
#'
#' ## Plot the entire interaction
#' song.PlotSongs(c)
#'
#' ## Change the line weight
#' song.PlotSongs(c, line.wt=10)
#'
#' ## Plot the songs occuring between 02m 30s and 03m 00s
#' song.PlotSongs(c, start.time=150, end.time=180, line.wt=10)
#'
#' ## Plot the entire interaction in 30 second panels
#' song.PlotSongs(c, breakpt=30)
#'
#' @import ggplot2
#' @export

song.PlotSongs <- function(indivs, start.time = NA, end.time = NA,
                           breakpt = NA, line.wt = 5){
  all.songs <- matrix(0, 0, 2)
  all.names <- character(0)
  ## extract start and end times from individual performance stats
  for (i in 1:length(indivs)){
    all.songs <- rbind(all.songs, indivs[[i]]$songs)
    all.names <- c(all.names, rep(indivs[[i]]$ID, indivs[[i]]$songs.num))
  }
  my.df <- data.frame(Individuals = all.names, Start = all.songs[,1],
                      End = all.songs[,2])
  ## check if user specified a time range
  if (is.na(start.time)){
    start.time <- min(my.df$Start)
  }
  if (is.na(end.time)){
    end.time <- max(my.df$End)
  }
  ## remove data outside of desired time range
  my.df <- my.df[which(my.df$Start >= start.time & my.df$End <= end.time),]
  ## check if user specified a breakpoint
  if (is.na(breakpt)){
    breakpt <- ceiling(end.time)
  }
  ## add bins for faceting
  num.bins <- ceiling(end.time/breakpt)
  bins <- c(0:num.bins)
  ## find the breaks and adjust for start.time
  tmp.breaks <- bins*breakpt
  breaks <- tmp.breaks + start.time
  ## put songs into bins
  group <- numeric()
  for(i in 1:length(my.df$Start)){
    for (j in 1:(length(breaks)-1)){
      if(my.df$Start[i] >= breaks[j] && my.df$Start[i] < breaks[j+1]){
        group[i] <- j
      }
    }
  }
  my.df <- data.frame(my.df, group)
  ## make dummy data to set x-axis limits
  if (breakpt == ceiling(end.time)){
    ## if no breakpoint specified, use start and end times
    dummy.start <- start.time
    dummy.end <- end.time
    dummy.df <- data.frame(Individuals=all.names[1], Start=dummy.start,
                           End=dummy.end, group=1)
    x.lab <- "Time (in seconds)"
    ## do not modify axis text
    x.line <- element_text(colour="black")

    ## if specified, use breaks
  } else{
    dummy.start <- breaks[1:(length(breaks)-1)]
    dummy.end <- dummy.start + breakpt
    dummy.group <- bins[2:length(bins)]
    dummy.df <- data.frame(Individuals=all.names[1], Start=dummy.start,
                           End=dummy.end, group=dummy.group)
    ## create x axis label
    x.lab <- paste("Time (panels represent ", breakpt, "second intervals)")
    ## remove axis text
    x.line <- element_blank()
  }
  ## remove dummy data outside of desired time range
  dummy.df <- dummy.df[which(dummy.end >= start.time &
                               dummy.start <= end.time),]
  ## build ggplot object
  songs.plot <- ggplot(data = my.df, aes(x = Individuals, ymin = Start,
                                         ymax = End, colour = Individuals)) +
    geom_linerange(size = I(line.wt)) +  geom_blank(data=dummy.df) +
    facet_grid(group ~ ., scales="free", space="free") +
    scale_y_continuous(name=x.lab) + coord_flip() + theme_bw() +
    theme(axis.text.x=x.line, legend.position = "none",
          strip.text.y = element_blank())
  return(songs.plot)
}

#' @title Plot the observed and expected amounts of song overlap.
#'
#' @description
#' \code{song.PlotResultsDensity} creates a \code{\link{ggplot}} object that
#' depicts the observed amount of overlap relative to the expected amount for
#' each possible pair of individuals. This function can be used to visualize
#' the output generated by \code{\link{song.Simulate}}.
#'
#' @param results A list created by \code{\link{song.Simulate}} that contains
#' the observed and expected amounts of overlap for an interaction.
#'
#' @return
#' \code{song.PlotResultDensity} returns a \code{\link{ggplot}} object in which
#' each panel of the plot represents a pairwise interaction. Each row (and fill
#' color) specifies the reference individual. Each column (and line color)
#' specifies the target individual. Within each panel, the observed amount of
#' overlap (dashed line) is plotted relative to the null distribution (filled
#' curve). The p-value corresponding to each pairwise interaction appears in the
#' upper right corner of each panel.
#'
#' @examples
#' ## Black-capped chickadees
#' c <- song.FromDataObj(chickadees)
#' c.rand <- song.Simulate(c, 100, song.TimeOverlap,
#'                         song.RandomizeSampleGaps)
#' song.PlotResultsDensity(c.rand)
#'
#' ## Rufous-and-white wrens
#' w <- song.FromDataObj(wrens)
#' w.rand <- song.Simulate(w, 100, song.TimeOverlap,
#'                         song.RandomizeKeepSongOrder)
#' song.PlotResultsDensity(w.rand)
#'
#' ## Long-tailed manakins
#' m <- song.FromDataObj(manakins)
#' m.rand <- song.Simulate(m, 100, song.TimeOverlap,
#'                         song.RandomizeSampleGaps)
#' song.PlotResultsDensity(m.rand)
#'
#' @import ggplot2
#' @import reshape2
#' @export

song.PlotResultsDensity <- function(results){
  ## create results data frame
  observed.df <- melt(results$observed)
  expected.df <- melt(results$expected)
  p.values.df <- melt(results$p.values)
  all.df <- observed.df
  all.df <- cbind(all.df, expected.df[,3])
  all.df <- cbind(all.df, p.values.df[,3])
  names(all.df) <- c("Reference", "Target", "Observed", "Expected", "p.value")
  ## create null distribution data frame
  density.df <- melt(results$randomized,
                     names = c("Reference", "Target", "randnum"))[,-3]
  names(density.df) <- c("Reference", "Target", "Overlap")
  indivs.names <- colnames(results$observed)
  density.df$Reference <- as.factor(indivs.names[density.df$Reference])
  ## make the order of the rows the same as in the p.values table
  density.df$Reference <- factor(density.df$Reference, colnames(results$p.values))
  density.df$Target <- as.factor(indivs.names[density.df$Target])
  ## make the order of the cols the same as in the p.values table
  density.df$Target <- factor(density.df$Target,
                                  colnames(results$p.values))
  ## create p-value labels
  indiv.names <- colnames(results$observed)
  tmp.refs <- character(0)
  tmp.targets <- character(0)
  tmp.maxdensity <- numeric(0)
  tmp.pvalue <- character(0)
  tmp.xvalue <- numeric(0)

  for (T in indiv.names){
    for (R in indiv.names){
      x <- density.df$Overlap[density.df$Reference == R &
                                density.df$Target == T]
      maxdens <- max(density(x, kernel = "gaussian", adjust = 1)$y)
      tmp.refs <- c(tmp.refs, R)
      tmp.targets <- c(tmp.targets, T)
      tmp.maxdensity <- c(tmp.maxdensity, maxdens)
      mypv <- all.df$p.value[all.df$Reference == R & all.df$Target == T]
      if (mypv < 0.01){
        tmp.pvalue <- c(tmp.pvalue, "p < 0.01")
      } else{
        if (mypv > 0.99){
          tmp.pvalue <- c(tmp.pvalue, "p > 0.99")
        } else{
          tmp.pvalue <- c(tmp.pvalue,
                          paste("p =", round(mypv,2), sep =""))
        }
      }
      tmp.xvalue <- c(tmp.xvalue, max(results$observed, results$randomized))
    }
  }
  p.df <- data.frame(Reference = tmp.refs, Target = tmp.targets,
                     xpos = tmp.xvalue, ypos = tmp.maxdensity,
                     mylabel = tmp.pvalue)
  ## adjust vertical alignment
  for (R in indiv.names){
    p.df$ypos[p.df$Reference == R] <- max( p.df$ypos[p.df$Reference == R])
  }

  ## build ggplot object
  overlap.density.plot <- ggplot(data = density.df,
                                 aes(x = Overlap, fill = Reference,
                                     colour = Target)) +
    geom_density(alpha = 0.2) +  theme_bw() + scale_y_continuous("Density") +
    geom_vline(data = all.df,
               aes(xintercept = Observed),
               colour = I("black"), linetype = 2, alpha = 0.5, size = 0.75) +                                                    geom_text(data = p.df, aes(x = xpos, y = ypos, label = mylabel), hjust = 1, vjust = 1, size = 4, col = "black") +
    facet_grid(Reference ~ Target, scales = "free_y", labeller = label_both) +
    theme(legend.position = "none")
  return(overlap.density.plot)
}
