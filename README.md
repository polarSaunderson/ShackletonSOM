## ShackletonSOM v1.1
### Investigating the variability of surface melt on Antarctic ice shelves
The scripts in this repository have been developed to investigate surface melt on the Shackleton Ice Shelf in East Antarctica as part of my PhD with the [Monash Ice Sheet Initiative](https://www.icesheet.org/) at [Monash University](https://www.monash.edu/science/schools/earth-atmosphere-environment/our-people).

This work has been published in [The Cryosphere](https://www.the-cryosphere.net/), and is available as an [open access article](https://doi.org/10.5194/tc-2022-94).

The scripts in this repository are shared in the interests of transparency and open science, and can be used to recreate the figures found in the journal article and its supplement.
Future work can also use this code to build on our findings and explore melt on other Antarctic ice shelves (see "Beyond Shackleton" below).

Please read the License for information about using these scripts, and make sure to reference the [TC paper](https://doi.org/10.5194/tc-2022-94) if you use them in your own work.
I'm happy to help if you have any questions.

<hr>

### General Overview of the Repository
This project was written in R using RStudio and uses the _renv_ package to help with portability / replicability.
Using _renv_ should be easy enough: after getting the repository on your system, launch the R Project, and it should call/install _renv_ automatically.
You can then use `renv::restore()` to recreate the environment; this function call will most likely need to download or update a number of packages, but does this on its own.
See [here](https://rstudio.github.io/renv/articles/renv.html#collaborating), [here](https://rstudio.github.io/renv/articles/collaborating.html), and [here](https://rpubs.com/glennwithtwons/reproducible-r-toolbox) for info about _renv_.

#### Scripts & File Names
There are three "types" of scripts in this repository: __dt__, __an__, and __su__, as well as __hp__ files and custom R functions.

Scripts necessary for __setting up__ other scripts begin with __su__. 
These scripts usually set global options and file paths etc., and are called by the __an__ and __dt__ scripts. 
All __su__ scripts are found in _.../R/setUp/_, and should only need to be accessed once when first working within the project.
Most importantly, you will need to set the file paths to raw data in _su02_ if you want to explore beyond the included data (see "Beyond Shackleton").

Scripts for processing and wrangling __data__ begin with __dt__, and are saved in _.../Data/Scripts/_.
The data scripts need to be run in order (i.e. _dt01_, then _dt02_, and so on).
The __dt__ scripts are not necessary for recreating the manuscript figures because the required data has already been pre-processed and is held in this repository under _.../Data/v01_Shackleton/_.\
However, the __dt__ scripts __do__ need to be run if investigating other shelves; the necessary raw data for such investigations first needs downloading, and can be accessed online at this [doi](https://doi.org/10.18709/perscido.2022.09.ds376).
Remember to set the file paths accordingly in _su02_ once you have these files on your system.

Scripts for __analysing__ the data begin with __an__, and are found in _.../Analysis/_.\
These __an__ scripts recreate the figures (see below) and are numbered to ease cross-referencing, but don't necessarily need to be run in the same order, as long as _an01_ has been run first; any other prerequisite scripts are noted at the top of a script.

When scripts are running, everything is organised into separate environments: 
  - dd is for Data produced in previous scripts 
  - ee is the Environment for the current script
  - ff is for File and Folder paths and names 
  - gg is for Global options that are referenced often

Files beginning with __hp__ can also be found in the repository, and are __helper__ files used to explain the repository.

#### User Options
At the top of each script (following an overview of the script), there are user options, like this: __u_variable__.\
These user options allow you to run the scripts with different input or to produce variations on the default plots.
For ease, user options can also be overwritten across the whole project by changing them in _su01_ or _su11_ rather than re-writing common ones over and over again in each script.

#### Functions
Custom functions are saved in _.../R/Functions_. 
The necessary functions at each stage should be loaded in by default by the __su__ scripts.
More information on the functions can be seen in the source files, or by using the docstring package: `docstring(_functionName_)`.
These functions have only been tested in their specific uses here - they may work elsewhere, but no promises!

<hr>

### Instructions
#### Recreating Published Figures
This repository contains the processed data for the Shackleton Ice Shelf, which is necessary to recreate the figures in the TC paper.
It is therefore not necessary to run any of the data scripts to recreate the figures.

To see which scripts create which figures, please look at: _.../Analysis/hp01_recreate_the_figures.md_

#### Beyond Shackleton
The data included in this repository (i.e. to recreate the published figures) has already been run through the __dt__ scripts for the Shackleton Ice Shelf.
However, the scripts are designed to be flexible enough to explore melt on any shelf in Antarctica.
To do this, you will need to download the binary melt data for all of Antarctica, which is available online [(doi)](https://doi.org/10.18709/perscido.2022.09.ds376).
Remember to set the paths to this accordingly in _su02_ once the files are on your system.

It should be noted that the scripts have predominantly been used with the settings necessary for the paper.
Whilst the scripts have been tested and should still work for other shelves and / or settings, occasional problems may be run into, and it is inevitable that some figures / plots will become awkward and potentially lose some of their legibility in the axes for example.
The scripts can also be noticeably slower for bigger shelves.

Let me know if you have any questions and I will try my best to help!

Cheers!
