
base::load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\4 - Run VAST on WCGBTS\\Less than or equal to 18 cm\\2018-03-29_PTRL_CW_DV_WCGBTS_Other_V4_AS_nx=600\\Image.RData")

# Load latest AreaGroup save file
# load("W:\\ALL_USR\\JRW\\R.Vanilla\\Petrale WCGBTS AreaGroup 03 Apr 2018 3_21PM.RData")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\4 - Run VAST on WCGBTS\\Petrale WCGBTS AreaGroup 05 Apr 2018 3_25PM.RData")

(CONTOUR <- c(T, F)[1])
(POLYGONS <- c(T, F)[1])
(PLOTLOWAREAS <- c(T, F)[1])


lib(INLA)

# To start fresh
# AreaGroup <- list()
# AreaGroup$One <- list()

# Two methods to find groups (groups of VAST underlying grid point areas identified as similar )

    # 1) Use identify() to find areas  (see Groups 1 and 2 below)
    # 2) Use selectPts() (see further below) - selectPts() is currently in JRWToolBox until it's moved to Imap with a help page

	# Do step 1 and 2 below

# Create GIS figure with plotGIS(), e.g. OR North & Central :
PNG <- F
CONTOUR <- NULL

# Repeat last sections of step 2 and following steps to finish
	
	
# Group 1
G <- 1
# Coastal area cA[[5]]
(AreaGroup$One$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Y < 45.8 & Petrale.Quant.Biomass.Stacked.Dpth$Y > 43.7 & 
                         Petrale.Quant.Biomass.Stacked.Dpth$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth, "Area"]))

# Take out the most westerly area (A:347) and make it its own group
AreaGroup$One$Areas  <- AreaGroup$One$Areas[!AreaGroup$One$Areas %in% 'A:347']
						 
plot(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AreaGroup$One$Areas , c("X", "Y")])
(AreaGroup$One$AreasPts <- Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AreaGroup$One$Areas, c("X", "Y", "Area")])


# Group 2 - only area A:347
G <- 2
# Coastal area cA[[5]]
(AreaGroup$Two$Areas <- 'A:347')
dev.new(); plot(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AreaGroup$Two$Areas , c("X", "Y")])
# for( i in 1:length(AreaGroup$Two$Areas)) {
#    points(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AreaGroup$Two$Areas[i], 
#	              c("X", "Y")], col = Col(length(AreaGroup$Two$Areas))[i], pch=19, cex=2)
# }
# (AreaGroup$Two$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AreaGroup$Two$Areas, c("X", "Y", "Area")]))
(AreaGroup$Two$AreasPts <- Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AreaGroup$Two$Areas, c("X", "Y", "Area")])
					

# Group 3
G <- 3
# Coastal area cA[[5]]
(AreaGroup$Three$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))
# Group 4
G <- 4
# Coastal area cA[[5]]
(AreaGroup$Four$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))
# Group 5
G <- 5
# Coastal area cA[[5]]
(AreaGroup$Five$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))
# Group 6
G <- 6
# Coastal area cA[[5]]
(AreaGroup$Six$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                      Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))
# Group 7
G <- 7
# Coastal area cA[[6]]
(AreaGroup$Seven$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))
# Group 8
G <- 8
# Coastal area cA[[6]]
(AreaGroup$Eight$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))						   
# Group 9
G <- 9
# Coastal area cA[[6]]
(AreaGroup$Nine$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))									   
# Group 10
G <- 10
# Coastal area cA[[6]]
(AreaGroup$Ten$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Year %in% 2003 & 
                                       Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS, c("X", "Y", "Area")]))





# --------- Step #1 - search over convex and resolution parameters -------

   Type <- c("Circle", "Ellipse", "Poly")[3]  # -99 for both resolution and convex implies circle

   dev.new() 
   par(mfrow =c(2,2))
       
   # For circles: convex = -99
   # for (k in c(-99)) {
   
   # For ellipses:  k > 0 implies ellipse parameters
   # for (k in 13/100) {
   
   # For polygons:
   # The lower the convex parameter (k in the loops) the tighter the polygon on the points.
   # for (k in c( -0.020, seq(-0.2, 0.2, by = 0.05))) {
   for (k in c(-0.15)) {
   # for (k in c(-0.02, -0.025, -0.03, -0.05, -0.075, -0.10, -0.15, -0.20, -0.25, -0.30)) {
   # for (k in c(-0.075, -0.10, -0.15)) {
    
     # for ( j in 5:2) {
     # for ( j in 30:5) {
	 # for ( j in seq(99, 39, by = -10)) {
     for ( j in c(seq(99, 39, by = -10), 30:5)) {
     # for ( j in 78:69) {     
	 
     # For circles: convex = -99 implies circle parameter, i.e. the "resolution parameter" is radius of the circle. (Picked so that the radius in close to one nm.)
     # for ( j in 0.01) {
      
     # For ellipses
     # for ( j in 0.01) {   
   	 
     # printf(j)
    
      AreaGroup[[G]]$Resolution <- j 
      AreaGroup[[G]]$Convex <- k
	  
      try({
   
        AREA <- AreaGroup[[G]]$AreasPts
		
      	Delta <- 0.04
   	    plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
   	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   	    	 
   	   switch(Type, 
   	     Circle = { AreaGroup[[G]]$Boundary <- circle.f(mean(AREA[1,1]), mean(AREA[1,2]) + 0.03, r = j/100, yaxis = T, facets.num = 40) }, 
   	   
   	     Ellipse = { Petrale.Spawning.Area.Polygons[[Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Sub.Area[N]]] <- 
   	                 ellipse(mean(AREA$X), mean(AREA$Y), j, k, col='black', theta = atan((AREA$Y[1] - AREA$Y[length(AREA$Y)])/(AREA$X[1] - AREA$X[length(AREA$X)])), figureAdj = FALSE) }, 
   	   
   	     Poly = { DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex = AreaGroup[[G]]$Convex, concave =  -0.40, res = AreaGroup[[G]]$Resolution )$loc
                  DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])   #Close the polygon
                  lines(DATA.inla.boundary, col='purple', type = 'o')
				})
       })
      }
  }

# End Step #1

# ------ Step #2 - apply convex and resolution parameters found in step 1 -----------

    AreaGroup[[G]]$Resolution <- 49
    AreaGroup[[G]]$Convex <- -0.085
	# AreaGroup[[G]]$Convex <- -99  # For a circle
    # AreaGroup[[G]]

    gof()
   
    AREA <- AreaGroup[[G]]$AreasPts
    Delta <- 0.04
   	plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
   	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   
   
	 switch( Type, 
	   Circle = { AreaGroup[[G]]$Boundary <- circle.f(mean(AREA[1,1]), mean(AREA[1,2]) + 0.03, r = AreaGroup[[G]]$Resolution, yaxis = T, facets.num = 40) }, 
	   
	   Ellipse = { Petrale.Spawning.Area.Polygons[[Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Sub.Area[N]]] <- 
	                ellipse(mean(AREA$X), mean(AREA$Y), Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Resolution[N], Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Convex[N],
 					col='black', theta = atan((AREA$Y[1] - AREA$Y[length(AREA$Y)])/(AREA$X[1] - AREA$X[length(AREA$X)])), figureAdj = FALSE) }, 
	   
	   Poly = { 
	    
         DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex = AreaGroup[[G]]$Convex, concave =  -0.40, res = AreaGroup[[G]]$Resolution )$loc
         DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])   #Close the polygon
         DATA.inla.boundary <- adjustPolygon(data.frame(x = DATA.inla.boundary[,1], y = DATA.inla.boundary[,2]))
         AreaGroup[[G]]$Boundary <- DATA.inla.boundary

	   })
     
   gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')	
    
# End Step #2   


# Look and/or Re-adjust shape of existing polygon
G
gof()
AREA <- AreaGroup[[G]]$AreasPts
Delta <- 0.04
plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
# Or plotGIS(...)	
#  polygon(AreaGroup[[G]]$Boundary	, col= col.alpha('purple', 0.25)) # Look only 
AreaGroup[[G]]$Boundary <- adjustPolygon(AreaGroup[[G]]$Boundary)  # Look and move
	
gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')	


# plotGIS(...)
Poly.New <- AreaGroup[[G]]$Boundary	
Poly.New <- adjustPolygon(Poly.New)

	
# Enlarge or shrink polygon
lib(polyclip) # ******* NEEDED FOR polyoffset() *******

G 
AREA <- AreaGroup[[G]]$AreasPts

Poly.New <- AreaGroup[[G]]$Boundary

Delta <- 0.04
plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
   	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
				
polygon(AreaGroup[[G]]$Boundary, col= col.alpha('purple', 0.25))				
Poly.New <- data.frame(adjustPolygon(polyoffset(list(x=Poly.New$x[-1], y=Poly.New$y[-1]), -0.007)[[1]]))

gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')		


# Final look for Enlarge or shrink polygon
plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
   	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
polygon(Poly.New, col= col.alpha('purple', 0.25))			
gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')		


# When finished with enlarging	
# Need to save often 
AreaGroup[[G]]$Boundary <- Poly.New
save(AreaGroup, file = "Petrale WCGBTS AreaGroup 05 Apr 2018 3_25PM.RData") 


# ==========================================================================================================================

# Look at polygons
windows() 
par(mfrow =c(2,2))

for (G in 1:10) {
   AREA <- AreaGroup[[G]]$AreasPts
   Delta <- 0.04
   plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
               main = paste("G =", G, "Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   polygon(AreaGroup[[G]]$Boundary, col=col.alpha('purple', 0.25))
}	


# --------------------- Saves for Melissa (saves from above)----------------------------

# Create Area Group list to share

AreaGroupsShareWCGBTS <- AreaGroup

for (i in 1:10) {
     AreaGroupsShareWCGBTS[[i]]$Areas <- NULL
	 AreaGroupsShareWCGBTS[[i]]$Resolution <- NULL
     AreaGroupsShareWCGBTS[[i]]$Convex <- NULL
}

# Check
str(AreaGroupsShareWCGBTS)

save( AreaGroupsShareWCGBTS, file = "Petrale AreaGroupsShareWCGBTS 06 Apr 2018.RData")

# ----------------------------------------------------------------------------------













	

