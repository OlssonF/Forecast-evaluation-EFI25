# Forecast-evaluation-EFI25

This repository contains the materials produced for and presented at the Ecological Forecasting Initiative Conference 2025. The materials contain a workshop tutorial that introduces concepts of ecological forecast evaluation and synthesis.

These materials were developed by Freya Olsson, Caleb Robbins, and Quinn Thomas.

## Pre-workshop instructions

### Setting up an R environment

R version 4.2 is required to run the code in this workshop. You should also check that your Rtools is up to date and compatible with R 4.2, see (<https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html>). Although instructions assume users have Rstudio installed, this is not a strict requirement.

The following packages need to be installed using the following code.

```{r}
install.packages('tidyverse') # collection of R packages for data manipulation, analysis, and visualisation
install.packages('lubridate') # working with dates and times
install.packages('arrow') # accessing remote filesystems and reading/writing parquet files
```

### Get the code

There are 3 options for getting the code locally so that you can run it, depending on your experience with Github/Git you can do one of the following:

1.  **Fork (recommended)** the repository to your Github and then clone the repository from your Github repository to a local RStudio project. This will allow you to modify the scripts and push it back to your Github.

-   Find the fork button in the top right of the webpage --\> Create Fork. This will generate a copy of this repository in your Github.
-   Then use the \<\> Code button to copy the HTTPS link (from your Github!).
-   In RStudio, go to New Project --\> Version Control --\> Git.
-   Paste the HTTPS link in the Repository URL space, and choose a suitable location for your local repository --\> Create Project.
-   Open the .Rmd file

2.  **Clone** the workshop repository to a local RStudio project. Your local workspace will be set up and you can commit changes locally but they won't be pushed back to the Github repository.

-   Use the \<\> Code button to copy the HTTPS link.
-   In RStudio go to New Project --\> Version Control --\> Git.
-   Paste the HTTPS link in the Repository URL space, and choose a suitable location for your local repository --\> Create Project.
-   Open the .Rmd file

3.  **Download** the zip file of the repository code. You can save changes (without version control) locally.

-   Find the \<\> Code button --\> Download ZIP.
-   Unzip this to a location on your PC and open the `Forecast-evaluation-EFI25.Rproj` file in RStudio.

More information on forking and cloning in R can be found at [happygitwithr](https://happygitwithr.com/fork-and-clone.html), a great resource to get you started using version control with RStudio.

For the workshop you can follow along via the rmarkdown document (`tutorial/forecast_evaluation.Rmd`) or the md (`tutorial/forecast_evaluation.md`), both of which can be downloaded here or you can fork the whole repository.

## Workshop format

1.  **Introduction to scoring and evaluation**: We include the presentation given during the in-person workshop that provides a lot of the background and rationale for the evaluation of ecological forecasts and the use of large forecast catalogs for forecast synthesis.
2.  **Accessing NEON catalog**: this section is a quick introduction to the EFI-NEON Forecast Challenge catalog, which will be used to demonstrate forecast evaluation themes. A more in-depth look at the NEON forecasts from the Challenge can be found in the [Get_scores_tutorial](https://github.com/eco4cast/NEON-forecast-challenge-workshop/tree/main/Analyse_scores) as part of the [NEON Forecast Challenge Workshop](https://github.com/eco4cast/NEON-forecast-challenge-workshop/tree/main).
3.   **Calculating and plotting evaluation metrics**: A demonstration in R showing the plotting of forecast scores for single and multi-model comparisons.
4.  **Conducting forecast synthesis:** Practical considerations for conducting large-scale synthesis efforts using forecast catalogs (e.g. unequal forecasts, metadata, co-authorship). This section of the workshop was conducted as a short presentation with discussion and draws on expertise of the workshop leads.
