# Purpose

R-approach to analyze CMIP5 climate simulation data. Is designed as far as a functions set. Package-wrapping should be done in future. 

Processing of monthly-aggregated data to assess multidecadal dynamics of the (area-aggregated) data.

# Options

* Automated search and check-up of nc-files inside the specified folder
* Accurate temporal interpolation for calculation of the month/seasonal means
* Re-grid to a user-defined grid (to make possible inter-model comparison)
* Prompt visualization of climatological fields

# How to run

1. Prepare CMIP5 data files. CMIP5 simulation data are availavle via data distribution nodes for non-commercial research and educational purposes. We are deeply grateful to the World Data Center for Climate in Hamburg (https://cera-www.dkrz.de) for a granted access to the CMIP5 simulation data with a perfect user interface
2. Download the project files
3. Set the data directory and the results directory in the local-config "0_ProcessCMIP5_Config_local.R". It is convenient to set that file to --assume-unchanged in git
4. Extract the models meta data
5. Check that continuous time-series are present for each requires time span
6. Form list of spatial matrices for each model by requested time spans
7. Calculate ensemble estimations
8. Visualize!

# Acknowledgments

We highly acknowledge participating modeling groups of the CMIP5 project and the World Data Center for Climate in Hamburg for making a huge simulation work and granting access to its results.
