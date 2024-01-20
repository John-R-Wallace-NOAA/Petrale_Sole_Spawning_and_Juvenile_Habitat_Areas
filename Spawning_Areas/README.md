
Steps used:

1)  Obtain the Petrale sole Winter Fishery logbook data by following the repo:

      https://github.com/John-R-Wallace-NOAA/PacFIN_Logbook_Download_and_Cleanup

to oabtain the 'LB.ShortForm.No.Hake.Strat 13 Apr 2017.RData' data.

2) Apply Thornson's VAST to the Petrale sole winter fishery logbook data, using the code in:

       VAST Petrale CW NOCV Biomass n_x = 600.R

3) Compare the raw data (DatG) with VAST Grid results using Bubble plots:

       Compare VAST Grid Results with DatG using Bubble plots.R

This code outputs the plotted data using 'PlotResultsOnMap_Fn_JRW' which is a hacked version of Thorson's 'PlotResultsOnMap' function. 'Petrale.Results.Biomass.Stacked.Dpth.Rst' is that output row stacked [rbind()] by year.

4) The critera that is used to define the areas of interest and outputs the points and polygon results is in:

       Petrale Spatial Results Top 20, in at Least 40% Years Dpth Rst.R

   
The eleven defined Petrale sole spawning areas are in (an R list() of lists):

       Petrale AreaGroupsShare 11 Jan 2018.RData
       
Each spawning area has the areas points (AreasPts) from the VAST grid and the esitmated polygon that surrounds that area (Boundary).
