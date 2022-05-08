## ShackletonSOM
### Investigating the variability of surface melt on Antarctic ice shelves
Many ice shelves in Antarctic experience melt across their surface during the austral summer each year, but many scientific questions remain.
One set of key questions concerns the variability of melt, both in time and space - when does melt occur, where does it occur, and how much do these vary between years?

As part of my PhD with the [Monash Ice Sheet Initiative](icesheet.org) at [Monash University](https://www.monash.edu/science/schools/earth-atmosphere-environment/our-people), I am working on answers to these questions and others using satellite observations of surface melt.
The scripts in this repository have been developed in the first stage of my PhD and used in my analysis of melt on the Shackleton Ice Shelf in East Antarctica.
The scripts can be used to recreate the figures in the manuscript (doi & link to come!), and are also easily applied to other ice shelves (see "Beyond Shackleton").

Please read the License for information about using these scripts.

### General Overview of the Repository
This project was written in R using RStudio.
It uses the renv package to help with portability / replicability.
_renv_ should be easy enough - after getting the repository on your system, launch the R Project, and renv will update itself. 
You can then use renv::restore() to recreate the environment; this will most likely need to download packages / updates.
See [here](https://rstudio.github.io/renv/articles/renv.html#collaborating) and [here](https://rstudio.github.io/renv/articles/collaborating.html) for info about renv.

#### Scripts & File Names
Scripts for processing and wrangling __data__ begin with __dt00__, and are saved in _Data/Scripts/_.\
The data scripts need to be run in order (i.e. dt01, then dt02, and so on).\

Scripts for __analysing__ the data begin with __an00__, and are found in _Analysis/_.\
These are numbered, but don't need to be run in the same order.

Scripts necessary for __setting up__ other scripts begin with __su00__. 
These scripts usually set global options and file paths etc., and are called by the __an00__ and __dt00__ scripts. __su00__ scripts are found in _R/setUp_, and should only need to be accessed once at the start.

Files beginning with __hp__ can be found throughout the repository, and are __helper__ files.

#### User Options
At the top of each script, following an overview of the script, there are user options, which are variables like this: __u_variable__.

#### Functions
These scripts make use of some custom functions, which are saved in R/Functions. 
The functions should be loaded in by default by the __an00__ scripts.
More information on the functions can be seen in the source files, or by using the docstring package: docstring(functionName).
The functions have only been tested in their specific uses here - they may work elsewhere, but no promises!

### Instructions
#### Recreating Manuscript Figures
The data for the Shackleton Ice Shelf, which is investigated in the submitted manuscript, is stored within this repository.
It is therefore not necessary to run any of the data scripts to recreate the figures.

To see which scripts create which figures, please look at: _Analysis/hp01_recreate_the_figures.md_
