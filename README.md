# README

## Simualtion and analysis code for *Marine reserves can provide buffering against environmental fluctuations to overexploited but not sustainably harvested fisheries*

Contact: [dpkilduff@ucdavis.edu](dpkilduff@ucdavis.edu)


## Introduction 

This repo contains code for the submitted manuscript *Marine reserves can provide buffering against environmental fluctuations to overexploited but not sustainably harvested fisheries* by Will White, Patrick Kilduff, Louis Botsford, and Alan Hastings

## How to use

These instructions assum that the analyst has [installed R](https://cloud.r-project.org/) (R version 4.3.1 was used for this study) and optionally [RStudio](https://posit.co/download/rstudio-desktop/). 

Clone the project or download and unzip the project file from [GitHub](https://github.com/dpkode/mpa-resilience.git). 

To install all the packages used by the project, open a new R session and run the following lines from the REPL:

```
install.packages("renv")
renv::restore() # installs specified versions of all packages in the `renv.lock` file
```

then run

```
targets::tar_make() 
```

to run the simulations using the [R targets package](https://books.ropensci.org/targets/) for analytical pipelines.

Note: running 5 simulations for the parameter set used in the manuscript produces an approx. 29GB duckdb database with 12 tables.

After running the simulations, figures can be created by running the code in the file `ms_figures_supp_figs.qmd`. To render the file `ms_figures_supp_figs.qmd`, [install Quarto](https://quarto.org/docs/get-started/) or, if using RStudio, use the `Run --> Run all` () 

