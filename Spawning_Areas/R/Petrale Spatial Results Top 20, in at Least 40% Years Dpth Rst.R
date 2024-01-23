
lib(INLA)
lib(Imap)
lib(polyclip)
lib(geosphere)


movePolygon <- function(xy, col = "blue", alpha = 0.5, lty = 1, colBg = 'white', ...) {
    polygon(xy$x, xy$y, col = NA, lty = lty, ...)
    while(length(PtRow <- identify(xy, labels = "", n = 1)) == 1) {
       newPt <- locator(1)
       polygon(xy$x, xy$y, lty = lty, col = NA, border = colBg, ...)
           xy[PtRow,] <- newPt
       polygon(xy$x, xy$y, col = NA, lty = lty, ...)
    }
    polygon(xy$x, xy$y, col = col.alpha(col, alpha), lty = lty, ...)
    invisible(xy)
}

# -------------------------------------------------------------------------------------------------------------------------------------------

AreaGroup <- list()
AreaGroup$One <- list()

# Group 1
 G <- 1
AreaGroup$One$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 47.8 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"])
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$One$Areas , c("X", "Y")])
(AreaGroup$One$AreasPts <- Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$One$Areas, c("X", "Y", "Area")])


# Group 2
 G <- 2
(AreaGroup$Two$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 47.0 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 46.6 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Two$Areas , c("X", "Y")])
for( i in 1:length(AreaGroup$Two$Areas)) {
    points(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Two$Areas[i], 
	              c("X", "Y")], col = Col(length(AreaGroup$Two$Areas))[i], pch=19, cex=2)
}
(AreaGroup$Two$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Two$Areas, c("X", "Y", "Area")]))


# Group 3
 G <- 3
(AreaGroup$Three$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 46.6 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 46.4 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Three$Areas , c("X", "Y")])
(AreaGroup$Three$AreasPts <- Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Three$Areas, c("X", "Y", "Area")])


# Group 4
 G <- 4
(AreaGroup$Four$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 44.0 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 43.5 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Four$Areas , c("X", "Y")])
(AreaGroup$Four$AreasPts <- Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Four$Areas, c("X", "Y", "Area")])


# Group 5
 G <- 5
(AreaGroup$Five$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 43.5 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 43.2 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Five$Areas , c("X", "Y")])
(AreaGroup$Five$AreasPts <- Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Five$Areas, c("X", "Y", "Area")])


# Group 6
 G <- 6
(AreaGroup$Six$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 43.2 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 42.5 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Six$Areas , c("X", "Y")])
(AreaGroup$Six$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Six$Areas, c("X", "Y", "Area")]))
 

# Group 7
 G <- 7
(AreaGroup$Seven$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 41.0 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 40.6 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Seven$Areas , c("X", "Y")])
(AreaGroup$Seven$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Seven$Areas, c("X", "Y", "Area")]))


# Group 8
 G <- 8
(AreaGroup$Eight$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 40.65 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 40.5 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Eight$Areas , c("X", "Y")])
(AreaGroup$Eight$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Eight$Areas, c("X", "Y", "Area")]))


# Group 9
 G <- 9
(AreaGroup$Nine$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 40.65 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 40.5 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Nine$Areas , c("X", "Y")])
(AreaGroup$Nine$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Nine$Areas, c("X", "Y", "Area")]))


# Group 10
 G <- 10
(AreaGroup$Ten$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 46.0 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 45.5 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Ten$Areas , c("X", "Y")])
(AreaGroup$Ten$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Ten$Areas, c("X", "Y", "Area")]))

# Group 11
 G <- 11
(AreaGroup$Eleven$Areas <- unique(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y < 45.5 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Y > 45.0 & 
                         Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, "Area"]))
plot(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Eleven$Areas , c("X", "Y")])
(AreaGroup$Eleven$AreasPts <- selectPts(Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Year %in% 1981 & Petrale.Quant.Biomass.Stacked.Dpth.Rst$Area %in% AreaGroup$Eleven$Areas, c("X", "Y", "Area")]))



# --------- Step #1 --------

   Type <- c("Circle", "Ellipse", "Poly")[3]   # -99 for both resolution and convex implies circle
  
   dev.new() 
   par(mfrow =c(2,2))
   
   
   # The lower the convex parameter (k in the loops) the tighter the polygon on the points.
   # for (k in c( -0.020, seq(-0.2, 0.2, by = 0.05))) {
   for (k in c(-0.1)) {
   # for (k in c(-0.02, -0.025, -0.03, -0.05, -0.075, -0.10, -0.15, -0.20, -0.25, -0.30)) {
   # for (k in c(-0.075, -0.10, -0.15)) {
   
   # For circles: convex = -99
   # for (k in c(-99)) {
   
   # For ellipses:  k > 0 implies ellipse parameters
   # for (k in 13/100) {
   
     # for ( j in 30:5) {
     # for ( j in c(seq(99, 39, by = -10), 30:5)) {
     for ( j in 78:69) {     
	 
     # For circles: convex = -99 implies circle parameter, i.e. the "resolution parameter" is radius of the circle. (Picked so that the radius in close to on nm.)
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
   	     Circle = { Petrale.Spawning.Area.Polygons[[Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Sub.Area[N]]] <- circle.f(AREA[1,1], AREA[1,2], r = j, yaxis = T, facets.num = 40)
                     print(gdistMeasure(units='km'))   }, 
   	   
   	     Ellipse = { Petrale.Spawning.Area.Polygons[[Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Sub.Area[N]]] <- 
   	                ellipse(mean(AREA$X), mean(AREA$Y), j, k, col='black', theta = atan((AREA$Y[1] - AREA$Y[length(AREA$Y)])/(AREA$X[1] - AREA$X[length(AREA$X)])), figureAdj = FALSE) }, 
   	   
   	     Poly = { DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex = AreaGroup[[G]]$Convex, concave =  -0.40, res = AreaGroup[[G]]$Resolution )$loc
                  DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])   # Close the polygon
                  lines(DATA.inla.boundary, col='purple', type = 'o')
				  # lines(DATA.inla.boundary, col='purple')
   	     })
       })
      }
  }


# ------ Step #2 -----------

    AreaGroup[[G]]$Resolution <- 78
    AreaGroup[[G]]$Convex <- -0.1
    # AreaGroup[[G]]

    gof()
   
    AREA <- AreaGroup[[G]]$AreasPts
    Delta <- 0.04
   	plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
   	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   
   
	 switch(Type, 
	   Circle = { Petrale.Spawning.Area.Polygons[[Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Sub.Area[N]]] <- circle.f(AREA[1,1], AREA[1,2], r = j, yaxis = T, facets.num = 40)
                  print(gdistMeasure(units='km'))   }, 
	   
	   Ellipse = { Petrale.Spawning.Area.Polygons[[Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Sub.Area[N]]] <- 
	                ellipse(mean(AREA$X), mean(AREA$Y), Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Resolution[N], Top.Areas.80.by.Year.at.Least.40prct.of.Years.Sub$Convex[N],
 					col='black', theta = atan((AREA$Y[1] - AREA$Y[length(AREA$Y)])/(AREA$X[1] - AREA$X[length(AREA$X)])), figureAdj = FALSE) }, 
	   
	   Poly = { 
         DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex = AreaGroup[[G]]$Convex, concave =  -0.40, res = AreaGroup[[G]]$Resolution )$loc
         DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])   #Close the polygon
         DATA.inla.boundary <- Imap::adjustPolygon(data.frame(x = DATA.inla.boundary[,1], y = DATA.inla.boundary[,2]), col = 'purple')
         AreaGroup[[G]]$Boundary <- DATA.inla.boundary

	   })
     
   gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')	
    

# Step #3    Re-adjust shape of existing polygon
   G  # Check Group
   gof()
   AREA <- AreaGroup[[G]]$AreasPts
   Delta <- 0.04
   plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
               main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   AreaGroup[[G]]$Boundary <- Imap::adjustPolygon(AreaGroup[[G]]$Boundary, col='purple')
   	
   gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')	

	
# Step #4   Enlarge or shrink polygon
   G # Check Group
   AREA <- AreaGroup[[G]]$AreasPts
   
   Poly.New <- AreaGroup[[G]]$Boundary
   Delta <- 0.04
   plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
      	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   polygon(AreaGroup[[G]]$Boundary, col= col.alpha('purple', 0.25))				
   Poly.New <- data.frame(Imap::adjustPolygon(polyclip::polyoffset(list(x=Poly.New$x[-1], y=Poly.New$y[-1]), -0.014)[[1]]))
   
   gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')	
	

# Step #5   Final look for Enlarge or shrink polygon
   plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
      	            main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   polygon(Poly.New, col= col.alpha('purple', 0.25))			
   gdistMeasure(units='km'); gdistMeasure(units='km'); gdistMeasure(units='km')		
   

# Step #6   Save when finished with enlarging	
   AreaGroup[[G]]$Boundary <- Poly.New
  
  

# Look at polygons
dev.new() 
par(mfrow =c(2,2))

for (G in 1:11) {
   AREA <- AreaGroup[[G]]$AreasPts
   Delta <- 0.04
   plot( AREA[,-3], xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
               main = paste("Res =", AreaGroup[[G]]$Resolution, "Convex =", AreaGroup[[G]]$Convex ))
   polygon(AreaGroup[[G]]$Boundary, col=col.alpha('purple', 0.25))
}	


# Reorder the groups
AreaGroup.OLD <- AreaGroup
AreaGroup$Four <- AreaGroup.OLD$Ten
AreaGroup$Five <- AreaGroup.OLD$Eleven
AreaGroup$Six <- AreaGroup.OLD$Four
AreaGroup$Seven <- AreaGroup.OLD$Five
AreaGroup$Eight <- AreaGroup.OLD$Six
AreaGroup$Nine <- AreaGroup.OLD$Seven
AreaGroup$Ten <- AreaGroup.OLD$Eight	
AreaGroup$Eleven <- AreaGroup.OLD$Nine	


c(AreaGroup[[1]]$Boundary[1,2], names(AreaGroup)[1])
c(AreaGroup[[2]]$Boundary[1,2], names(AreaGroup)[2])
c(AreaGroup[[3]]$Boundary[1,2], names(AreaGroup)[3])
c(AreaGroup[[4]]$Boundary[1,2], names(AreaGroup)[4])
c(AreaGroup[[5]]$Boundary[1,2], names(AreaGroup)[5])
c(AreaGroup[[6]]$Boundary[1,2], names(AreaGroup)[6])
c(AreaGroup[[7]]$Boundary[1,2], names(AreaGroup)[7])
c(AreaGroup[[8]]$Boundary[1,2], names(AreaGroup)[8])
c(AreaGroup[[9]]$Boundary[1,2], names(AreaGroup)[9])
c(AreaGroup[[10]]$Boundary[1,2], names(AreaGroup)[10])
c(AreaGroup[[11]]$Boundary[1,2], names(AreaGroup)[11])


save(AreaGroup, file = "Petrale AreaGroup 10 Jan 2018 4_48PM.RData")  # Intermittent  save so that work above is not lost
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale AreaGroup 10 Jan 2018 4_48PM.RData")
	

# ======================== Biomass Proportion by Area within Year ====================================

# Check for duplicate points within Year and Area - *** Looks Good ***
dim(Petrale.Results.Biomass.Stacked.Dpth.Rst) # 141400      5
change(Petrale.Results.Biomass.Stacked.Dpth.Rst)
length(!duplicated(paste(Area, Year, X, Y))) # 141400

# Some loner points were not selected so the N from GroupsNumArea.df needs to be used in the weighting


GroupsNumArea.df <- NULL
for( i in 1:11) 
     GroupsNumArea.df <- rbind(GroupsNumArea.df, data.frame(AreaGroup[[i]]$AreasPts, Group = names(AreaGroup)[[i]]))

Petrale.Spawning.Groups <- NULL	 
for(i in 1981:2015) 	{ 	 
	 TMP <- match.f(GroupsNumArea.df, Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Year %in% i,], "Area", "Area", c("Biomass.mt", "Year"))
	 Petrale.Spawning.Groups <- rbind(Petrale.Spawning.Groups, TMP)
}	
Petrale.Spawning.Groups <- renum(Petrale.Spawning.Groups)


Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst <- NULL 
 for ( i in 1981:2015) {
   Petrale.Spawning.Groups.Year <- Petrale.Spawning.Groups[Petrale.Spawning.Groups$Year %in% i,]
   Petrale.Weighted.Biomass.Total.by.Area <- data.frame(Year = i, aggregate(list(BiomassWeightedTotOverAreaWithinYear = Petrale.Spawning.Groups.Year$Biomass.mt), list(Group = Petrale.Spawning.Groups.Year$Group), sum))
   Petrale.Weighted.Biomass.Total.by.Area$PropBiomass <- Petrale.Weighted.Biomass.Total.by.Area$BiomassWeightedTotOverAreaWithinYear/sum(Petrale.Weighted.Biomass.Total.by.Area$BiomassWeightedTotOverAreaWithinYear)		 
   Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst  <- rbind(Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst , Petrale.Weighted.Biomass.Total.by.Area)
}

Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst[1:4,]


change(Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst)
Table(Group, Year)
aggregate(List(PropBiomass), List(Year), sum)

dir.create(Dir, recursive =T)
png(paste0(Dir, "BioWtByYearAndGroup.png"), width = 800, height = 800, bg = 'grey')
# dev.new()
xyplot(BiomassWeightedTotOverAreaWithinYear ~ Year | ordered(Group, c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Eleven")), data = Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst, 
                  ylab = "Biomass (mt) Weighted Total", as.table=T, type='o')				

png(paste0(Dir, "BioWtByYearAndGroup- Big Font.png"), width = 800, height = 800, bg = 'grey')				  
xyplot(BiomassWeightedTotOverAreaWithinYear ~ Year | ordered(Group, c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Eleven")), data = Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst, 
                  ylab = list(label="Biomass (mt) Weighted Total", cex=1.3), xlab = list(label="Year", cex=1.3), scales=list(x=list(cex=1.1), y=list(cex=1.1)), as.table=T, type='o', cex=1.1)
				  
  

png(paste0(Dir, "PropBioByYearAndGroup.png"), width = 800, height = 800, bg = 'grey')
# dev.new()
xyplot(PropBiomass ~ Year | ordered(Group, c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Eleven")), data = Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst, 
                  ylab = "Proportion Biomass (sums to one over groups within years)", as.table = TRUE, type = 'o')


 
# --------- Density --------- 
# Would need to do an average weighted biomass within year in: Petrale.Spawning.Groups.PropBiomass.by.Year.Dpth.Rst - below is old version
# Petrale.Spawning.Areas.Prop.by.Year.Meta$Kg.per.Hectare <- Petrale.Spawning.Areas.Prop.by.Year.Meta$Biomass*1000/Petrale.Spawning.Areas.Prop.by.Year.Meta$Area.h
	
#  Some missing code here it seems .... , but the average weighted biomass within year was done:
save(Petrale.Spawning.Groups.PropBiomass.by.Year.Dpth.Rst, file = 'Petrale.Spawning.Groups.PropBiomass.by.Year.Dpth.Rst.RData')



# ----------------  Create metadata information for each polygon -------------------------

# geosphere::areaPolygon(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))/(1852*60*1852*60)  # 0.9968515  Test to check output is in square meters

(Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta <- data.frame(Group = names(AreaGroup), centroidLon = NA, centroidLat = NA, Area.h = NA, minLat = NA, maxLat = NA))


for( i in Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Group)  {
   
   Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidLon[Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Group %in% i] <- centroid(AreaGroup[[i]]$Boundary)[1]
   Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidLat[Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Group %in% i] <- centroid(AreaGroup[[i]]$Boundary)[2]
   Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Area.h[Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Group %in% i] <- geosphere::areaPolygon(AreaGroup[[i]]$Boundary)/100^2  # Sq meters converted to hectares 
   Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$minLat[Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Group%in% i] <- min(AreaGroup[[i]]$Boundary[,2])
   Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$maxLat[Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$Group %in% i] <- max(AreaGroup[[i]]$Boundary[,2])
}

Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidDepth.m <- depthMeters(Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta[,c("centroidLon", "centroidLat")])
Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta



# ------------- Polygons by State -------------------	
	 
Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidInState <- 'WA'
Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidInState[ Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidLat < 46 + 16/60] <- 'OR'
Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidInState[ Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidLat < 42] <- 'CA'
	 
Table(Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta$centroidInState)

CA OR WA 
 3  5  3 

	
# Need to keep metadata table separate from AreaGroup list

	
    
# --------------------- Saves for Melissa ----------------------------

# Create Area Group list to share

AreaGroupsShare <- AreaGroup

for (i in 1:11) {
     AreaGroupsShare[[i]]$Areas <- NULL
	 AreaGroupsShare[[i]]$Resolution <- NULL
     AreaGroupsShare[[i]]$Convex <- NULL
}

AreaGroupsShare$One <- list(AreasPts = AreaGroupsShare$One$AreasPts, Boundary = AreaGroupsShare$One$Boundary)


# Push out the polgon figures on a 2 X 2 grid out to png
Dir <- "Figs/Top 80P in at least 40P Years, Region = Other/"
png(paste0(Dir, "Polygons%03d.png"), width = 800, height = 800, bg = 'grey')
par(mfrow =c(2,2))

for (G in 1:11) {
   AREA <- AreaGroupsShare[[G]]$AreasPts[,-3]
   Delta <- 0.04
   plot( AREA, xlim = c(min(AREA$X) - Delta, max(AREA$X) + Delta), ylim = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), 
               main = paste("Res =", AreaGroupsShare[[G]]$Resolution, "Convex =", AreaGroupsShare[[G]]$Convex ))
   polygon(AreaGroupsShare[[G]]$Boundary, col=col.alpha('purple', 0.25))
}	
dev.off()


# Use the Imap package to see the polygons next to the local coastline with a browser window
# optOld <- options(browser = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
optOld <- options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
for (G in 8) {
   AREA <- AreaGroupsShare[[G]]$AreasPts[,-3]
   Delta <- 0.25
   JRWToolBox::browsePlot('
     Imap::imap(list(world.h.land, AreaGroupsShare[[G]]$Boundary), longrange = c(min(AREA$X) - Delta * 2.5, max(AREA$X) + Delta * 3.5),
          latrange = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), poly = c("grey40", col.alpha("purple", 0.25)), zoom = FALSE,)
     points(AREA, cex = 0.5)
   ')
}	
options(optOld)


# Note that the use of the 'file' argument in JRWToolBox::browsePlot() to save the figure with that file name 
# If your browser option is set to Chrome (or MS Edge) and opens several tabs each with the text in between each space then upgrade Chrome 
#          (https://support.google.com/chrome/thread/76204149/bug-chrome-can-t-open-local-files-with-spaces-in-their-path?hl=en)
# Another option is ensure the entire path to your figures does not contain any spaces.

# options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# options(browser = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
# options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe") 

# A 'NULL' browser option will use a photo viewer (at least for me)  but it's not tabbed like a browser
optOld <- options(browser = NULL)
for (G in 6) {
   AREA <- AreaGroupsShare[[G]]$AreasPts[,-3]
   Delta <- 0.25
   JRWToolBox::browsePlot('
     Imap::imap(list(world.h.land, AreaGroupsShare[[G]]$Boundary), longrange = c(min(AREA$X) - Delta * 2.5, max(AREA$X) + Delta * 3.5),
          latrange = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), poly = c("grey40", col.alpha("purple", 0.25)), zoom = FALSE,)
     points(AREA, cex = 0.5)
   ', file = paste0("Petrale Spawning Area ", G, ".png"))
}	
options(optOld)


# If the NOAA's National Centers for Environmental Information is serving up the NOAA's U.S. Coastal Relief Model for the contiguous U.S correctly (has been spotty as of Jan 2024) then using Imap::plotRAST() will show the bathymetry of the ocean floor
for (G in 11) {    # Just try one area to start
   AREA <- AreaGroupsShare[[G]]$AreasPts[,-3]
   Delta <- 0.25
   JRWToolBox::browsePlot('
     Imap::plotRAST(AREA, list(world.h.land, AreaGroupsShare[[G]]$Boundary), longrange = c(min(AREA$X) - Delta * 2.5, max(AREA$X) + Delta * 3.5),
          latrange = c(min(AREA$Y) - Delta, max(AREA$Y) + Delta), col.poly = c("grey40", col.alpha("purple", 0.65)))
   ')
}	


save(AreaGroupsShare, file = "Petrale AreaGroupsShare 11 Jan 2018.RData")
save(Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst, file = 'Petrale.Spawning.Groups.Prop.Area.Within.Year.Dpth.Rst 16 Jan 2018.RData')
save(Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta, file = 'Petrale.Spawning.Area.Polygons.Dpth.Rst.Meta 11 Jan 2018.RData')

# ----------------------------------------------------------------------------------













	

