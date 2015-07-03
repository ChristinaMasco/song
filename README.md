song
====

Animals often communicate interactively, adjusting the structure or timing of
their signals in response to those of nearby conspecifics. During such 
interactions, individuals may produce signals at the same time, overlapping 
each other's signals. In many cases, however, it is unclear whether this 
overlap is the result of intention or chance. To address this issue, we have 
developed the <b>Song Overlap Null model Generator (SONG)</b> package for R. 
For a given interaction, SONG calculates the expected amount of chance overlap, 
then compares the observed amount of overlap to this expectation to generate a 
p-value.

<h1>Installing song</h1>
To download this package, first install the library "devtools".
Next, open the R console and type:

> library(devtools)

> install_github("ChristinaMasco/song")

To load the package, type:

> library(song)

<h1>Using SONG</h1>
The input for the SONG package is a tab-delimited text file that contains the 
start time, end time, and singer identity for each song during an interaction.
Once your data have been organized in this fashion, use the function 
<code>song.ReadSongList</code> to build a list of performance statistics for
each individual in your data set. Next, use the function 
<code>song.Simulate</code> to calculate the observed and expected amounts of 
overlap in the interaction. To plot the results of the analysis, use the 
function <code>song.PlotResultsDensity</code>. To learn more about each of 
these functions, type <code>"?"</code> followed by the name of the function.

Additional features of the package include:
<ul>
<li> Two methods for calculating overlap:
  <ul>
  <li><code>song.TimeOverlap</code></li>
  <li><code>song.NumOverlap</code></li>
  </ul>
<li> Three randomization methods:
  <ul>
  <li><code>song.RandomizeSampleGaps</code></li>
  <li><code>song.RandomizeKeepGaps</code></li>
  <li><code>song.RandomizeKeepSongOrder</code></li>
  </ul>
<li><code>song.DutyCycleMethod</code>: the duty cycle method for calculating 
chance overlap (Ficken, Ficken, & Hailman 1974).</li>
</ul>

Please direct any questions to Christina Masco at cmasco@uchicago.edu.

<h3>Reference information</h3>
Masco C, Allesina S, Mennill DJ, Pruett-Jones, S. (in review).  The Song 
Overlap Null model Generator (SONG): a new tool for distinguishing between
random and non-random song overlap.
