
Steps used:

1) The Petrale sole Winter Fishery logbook data, 'LB.ShortForm.No.Hake.Strat 13 Apr 2017.RData', was obtained by following (an old version of) this repo:

      https://github.com/John-R-Wallace-NOAA/PacFIN_Logbook_Download_and_Cleanup

2) Thorson's [VAST](https://github.com/James-Thorson-NOAA/VAST]) was applied to the data, using the code in:

       VAST Petrale CW NOCV Biomass Other Regions n_x = 300.R

3) The raw data (DatG) was compared with VAST grid results using bubble plots:

       Compare VAST Grid Results with DatG using Bubble plots.R

This code also outputs the plotted data using 'PlotResultsOnMap_Fn_JRW' which is a hacked version of Thorson's 'PlotResultsOnMap' function. 'Petrale.Results.Biomass.Stacked.Dpth.Rst' is that output row stacked [rbind()] by year.

4) The selected criteria define areas (within 'Petrale.Results.Biomass.Stacked.Dpth.Rst') of interest and the points and polygon results are output:

       Petrale Spatial Results Top 20, in at Least 40% Years Dpth Rst.R

   
The eleven defined Petrale sole spawning areas (an R list() of lists) are in this sub-repo here:

       Polygon_Area_Results > Petrale AreaGroupsShare 11 Jan 2018.RData
       
Each spawning area within the list has the areas points (AreasPts) from the VAST grid and the estimated polygon that surrounds that area (Boundary).

The code that plots these areas in various ways starts around line 411 of 'Petrale Spatial Results Top 20, in at Least 40% Years Dpth Rst.R'. For convenience, a snippet of code that plots those areas in a simple way is given below:


     load('Petrale AreaGroupsShare 11 Jan 2018.RData')
     
     par(mfrow =c(2,2))
     
     for (G in 1:11) {
        AREA <- AreaGroupsShare[[G]]$AreasPts[,-3]
        Delta <- 0.04
        plot( AREA, xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
                    main = paste("Res =", AreaGroupsShare[[G]]$Resolution, "Convex =", AreaGroupsShare[[G]]$Convex ))
        polygon(AreaGroupsShare[[G]]$Boundary, col=col.alpha('purple', 0.25))
     }	
    
