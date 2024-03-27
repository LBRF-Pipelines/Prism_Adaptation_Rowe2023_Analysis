# Prism Adaptation & Motor Imagery Analysis
Analysis code for a study examining if internal models are updated during motor imagery-based practice

## Dependencies

To install the R packages required for runnning this analysis pipline, run the following command at an R prompt: 

```r
install.packages(
  c("data.table", "dplyr", "tidytable", "ggplot2", "Rmisc", "afex", "emmeans", "performance", "car")
  )
```

## Running the pipeline

First, to run the pipeline, you will need to place the contents of the ```Data folder``` from the project's OSF repository (found [here](https://osf.io/6tv5m/files/osfstorage)) in the pipeline's ```_Data folder```. 

Then, set the working directory in R to the ```Prism_Adaptation_Rowe2023_Analysis folder``` and run the following source commands in sequence.

```r
# Import all task and demographic data for the study
source("./_Scripts/0_import.R")

# Run two-way ANOVA
source("./_Scripts/1_twoway_ANOVA.R")

# Summarize demographic data
source("./_Scripts/2_demographic_analysis.R")
```
