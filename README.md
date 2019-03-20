# +++++ Scripts for project ATMOperm

Functions and colors are in the directory "work_files".

#################################################
# Sampling
#################################################
### create-smet-spinup.R
Create an smet-file for snowpack-runs, including a spinup. Length of spinup can be adjusted.

### create-sno-ini-smet.R
Prepare sampling over the selected parameters using DiceDesign() and prepare .sno, .ini and .smet-files (without spinup).

### sampling.R
Run the sampling for 10000 different simulations in a multicore-environment and create basic output for analyzing it later.

### sampling-evaluation.R
Analyze the active layer thickness of the sampling-runs and calculate RMSE

### profile-analysing.R
Calculate RMSE for soil temperature profiles and create graphics.


#################################################
# Scenarios
#################################################
### scenarios-smet.R
Prepare smet-files for the scenarios including spinup.

### scenarios-ini.R
Prepare ini-files for the scenarios

### scenarios-run.R
Run the scenarios in a multicore-environment and create basic output for analyzing it later.

### scenarios-evaluation.R
Evaluate the scenarios, creating also graphics


#################################################
# Miscellaneous
#################################################
### smet-comparison
Creates graphics out of two smet-files for comparison reasons.

### alt-calculator.R
Calculate Active Layer Thickness out of Observations.

#################################################
# Functions
#################################################
### SnExtract.sh
Written by SLF. Extracts data from a pro-file (snowpack-output-file) to a text-file.

### f_alt.calculator.R
Calculates hourly, daily and yearly alt of a soil-temperature-dataset

### f_gaussfilter.R
Calculates gaussfiltered data out of a timeline

### f_pro.import.R
Imports data from the extracted data from a pro-file, which is done by SnExtract.sh.

### f_lin_fill.R
Function for linear interpolation of data gaps in time-series

### f_rmse.R 
Function for assessing model performance

### f_sno.create.R
Creates sno-files

### f_sno.create.spinup.R
Create sno-files for the spinup-runs

### f_sno.sampling.R
create sno-files for the sampling-runs

### sce.color.RData
Colors used for the scenario-plots