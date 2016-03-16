song
====

Animals often communicate interactively, adjusting the structure or timing of
their signals in response to those of nearby conspecifics. During such 
interactions, individuals may produce signals at the same time, overlapping 
each other's signals. In many cases, however, it is unclear whether this 
overlap is the result of intention or chance. To address this issue, we have 
developed the **Song Overlap Null model Generator (SONG)** package for R. 
For a given interaction, SONG calculates the expected amount of chance overlap, 
then compares the observed amount of overlap to this expectation to generate a 
p-value.

# Installing song
To download this package, first install the library "devtools".
Next, open the R console and type:

> library(devtools)

> install_github("ChristinaMasco/song")

To load the package, type:

> library(song)

# Using SONG
The input for the SONG package is a tab-delimited text file that contains the 
start time, end time, and singer identity for each song during an interaction.
Once your data have been organized in this fashion, use the function 
`song.FromTextFile` to build a list of performance statistics for
each individual in your data set. If you have already imported these data into
R, and they are stored in a matrix or data frame within the global environment,
instead use the function `song.FromDataObj`. 

Once this step is complete, use the function `song.Simulate` to 
calculate the observed and expected amounts of overlap in the interaction. To 
view the results of the analysis, use the function `song.Summarize`.
To learn more about each of these functions, type `?` followed by 
the name of the function.

The following is an example using the built-in data set, `wrens`:

> birds <- song.FromDataObj(wrens)  # use song.FromTextFile() for text files

> rndbirds <- song.Simulate(birds)

> song.Summarize(rndbirds)

Additional features of the package include:

* Two methods for calculating overlap:
 - `song.TimeOverlap`
 - `song.NumOverlap`
  
* Three randomization methods:
  - `song.RandomizeSampleGaps`
  - `song.RandomizeKeepGaps`
  - `song.RandomizeKeepSongOrder`
  
* The duty cycle method for calculating chance overlap (Ficken, Ficken, & 
Hailman 1974): `song.DutyCycleMethod`.

* Functions for visualization:
  - `song.PlotSongs` for viewing the relative timing of songs
  during an interaction.
  - `song.PlotResultsDensity` for plotting the observed and
  expected amounts of overlap.
  


Please direct any questions to Christina Masco at cmasco@uchicago.edu.

## Reference information
Masco C, Allesina S, Mennill DJ, Pruett-Jones, S. 2016.  The Song 
Overlap Null model Generator (SONG): a new tool for distinguishing between
random and non-random song overlap. Bioacoustics 25, 29-40.
