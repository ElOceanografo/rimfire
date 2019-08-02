Seasonal changes in the biomass, distribution, and patchiness of zooplankton and fish in four lakes in the Sierra Nevada, California
----------------
Samuel S. Urmy and Joseph D. Warren

Freshwater Biology 2019, 00: 1-18

DOI: https://doi.org/10.1111/fwb.13362

This repository contains the data and scripts needed to reproduce the analysis in this paper.
To run them, follow the steps below:

1. Clone this repository to your computer.
2. Make sure you have the necessary software installed. You will need
	1. R (http://r-project.org/) and the following libraries:
	  * `dplyr`
	  * `ggplot2`
	  * `ggrepel`
	  * `gridExtra`
	  * `gstat`
	  * `lubridate`
	  * `nlme`
	  * `reshape2`
	  * `scales`
	  * `tidyr`
	  * `viridis`
	2. Julia (http://julialang.org/) v0.7 or higher, and the following packages:
	  * `DataFrames`
	  * `DataFramesMeta`
	  * `Distributions`
	  * `FileIO`
	  * `SDWBA`
	3. GNU Make (should come pre-installed on Linux and MacOS).
3. Open a terminal and `cd` into the `analysis` directory
4. Type `make` and hit Enter, which will run all the scripts in the correct order and produce the figures.  The first time you run it, it will download several large shapefiles from the USGS, which may take a little while.
