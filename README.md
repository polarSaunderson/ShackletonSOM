## ShackletonSOM
### Investigating the variability of surface melt on Antarctic ice shelves
The scripts in this repository have been developed to investigate surface melt on the Shackleton Ice Shelf in East Antarctica as part of my PhD with the [Monash Ice Sheet Initiative](https://www.icesheet.org/) at [Monash University](https://www.monash.edu/science/schools/earth-atmosphere-environment/our-people).

This work is currently under review in The Cryosphere Discussions [(link)](https://doi.org/10.5194/tc-2022-94).
The scripts here are shared in the interests of transparency and open science, and can be used to recreate the figures found in the manuscript and supplement.
Future work can also build on this work to explore melt on other Antarctic ice shelves (see "Beyond Shackleton" below).

Please read the License for information about using these scripts, and reference the TCD paper if you use them in your own work.
Let me know if you have any questions!

### General Overview of the Repository
This project was written in R using RStudio.
It uses the renv package to help with portability / replicability.
Using _renv_ should be easy enough - after getting the repository on your system, launch the R Project, and run renv::init(). 
You can then use renv::restore() to recreate the environment; this will most likely need to download packages / updates.
See [here](https://rstudio.github.io/renv/articles/renv.html#collaborating), [here](https://rpubs.com/glennwithtwons/reproducible-r-toolbox), and [here](https://rstudio.github.io/renv/articles/collaborating.html) for info about renv.

#### Scripts & File Names
Scripts for processing and wrangling __data__ begin with __dt__, and are saved in _Data/Scripts/_.\
The data scripts need to be run in order (i.e. _dt01_, then _dt02_, and so on).\
These scripts are not necessary for recreating the manuscripts figures because the required data is already processed and saved here.
However, these scripts need to be run if investigating other shelves; the necessary raw data for such investigations will be published online upon final acceptance of the manuscript.

Scripts for __analysing__ the data begin with __an__, and are found in _Analysis/_.\
These scripts recreate the figures (see below) and are numbered to ease cross-referencing, but don't necessarily need to be run in the same order, as long as _an01_ has been run first; any other prerequisite scripts are noted at the top of a script.

Scripts necessary for __setting up__ other scripts begin with __su__. 
These scripts usually set global options and file paths etc., and are called by the __an__ and __dt__ scripts. 
All __su__ scripts are found in _R/setUp/_, and should only need to be accessed once when first working within the project.
Most importantly, you will need to set the file paths to raw data in _su02_ if you want to explore beyond the included data (see "Beyond Shackleton").

When scripts are running, everything is stored in environments: 
  - dd is for Data produced in previous scripts 
  - ee is the Environment for the current script
  - ff is for File and Folder paths and names 
  - gg is for Global options that are referenced often

Files beginning with __hp__ can be found in the repository, and are __helper__ files used to explain the repository.

#### User Options
At the top of each script, following an overview of the script, there are user options, which are variables like this: __u_variable__.\
User options can also be overwritten in _su01_ or _su11_ rather than re-writing common ones in each script if necessary.

#### Functions
These scripts make use of some custom functions, which are saved in _R/Functions_. 
The necessary functions should be loaded in by default by the __su__ scripts.
More information on the functions can be seen in the source files, or by using the docstring package: docstring(functionName).
The functions have only been tested in their specific uses here - they may work elsewhere, but no promises!

### Instructions
#### Recreating Manuscript Figures
This repository contains the processed data for the Shackleton Ice Shelf, which is necessary to recreate the figures in the TCD manuscript.
It is therefore not necessary to run any of the data scripts to recreate the figures.

To see which scripts create which figures, please look at: _Analysis/hp01_recreate_the_figures.md_

#### Beyond Shackleton
The data included here (i.e. to recreate the manuscript figures) has already been run through the __dt__ scripts for the Shackleton Ice Shelf.
However, the scripts are designed to be flexible enough to explore melt on any shelf in Antarctica.
The data necessary for this will be published online upon final acceptance of the manuscript.

It should be noted that the scripts have predominantly been used with the settings necessary for the paper.
Whilst they have been tested and should still work for other settings, occasional problems may be run into, and it is inevitable that some figures / plots will become awkward and potentially lose some of their legibility in the axes for example.\
The scripts can also be noticeably slower for bigger shelves.

If you have any questions, I will try my best to help! Just let me know :)

Thanks!
