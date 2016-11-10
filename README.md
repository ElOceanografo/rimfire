Rim Fire Analysis
----------------

Data and analysis scripts for the Rim Fire project.


All the scripts needed to reproduce the analysis in this paper are in the analysis folder.  To run them, follow the steps below:

1. Clone this repository to your computer.
2. Make sure you have the necessary software installed. You will need
	1. R (http://r-project.org/) and the following libraries:
	2. Julia (http://julialang.org/) v0.5 or higher, and the following packages:
	3. GNU Make (should come pre-installed on Linux and MacOS).
3. Open a terminal and `cd` into the `analysis` directory
4. Type `make` and hit Enter, which will run all the scripts in the correct order and produce the figures.  The first time you run it, it will download several large shapefiles from the USGS, which may take a little while.