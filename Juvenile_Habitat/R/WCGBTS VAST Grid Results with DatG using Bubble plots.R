

# Pick the region used in VAST
Region <- c("Other", "California_current")[1]
 

# Avoid my load(), which loads twice.

# --------- CA Current Region -----------
if(Region == "California_current")	{ 
    # 300 knots
    # base::load("W:\\ALL_USR\\JRW\\R.Vanilla\\2017-12-28_Petrale_CW_NOCV_Winter_nx=300\\Image.Rdata")
    # base::load("W:\\ALL_USR\\JRW\\R.Vanilla\\2018-01-03_Petrale_CW_NOCV_Winter_nx=300\\Image.Rdata")  # More restricted data
    
    # base::load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass Regions n_x = 300/2018-01-05_Petrale_CW_NOCV_Winter_CA_Curr_nx=300/Image.RData")  # More restricted data
	# DateFile <- "W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass Regions n_x = 300/2018-01-05_Petrale_CW_NOCV_Winter_CA_Curr_nx=300/"
}
    
# ----------- "Other" Region ---------
if(Region == "Other")	{  
   # base::load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/4 - Run VAST on WCGBTS/Less than or equal to 18 cm\2018-03-16_PTRL_CW_NCV_WCGBTS_Other_V3_AS_nx=600/Image.Rdata")  
   # DateFile <- "W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/4 - Run VAST on WCGBTS/Less than or equal to 18 cm\2018-03-16_PTRL_CW_NCV_WCGBTS_Other_V3_AS_nx=600"
   
   base::load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\4 - Run VAST on WCGBTS\\Less than or equal to 18 cm\\2018-03-29_PTRL_CW_DV_WCGBTS_Other_V4_AS_nx=600\\Image.RData")
   ##### Don't need a correct DateFile for PlotResultsOnMap_Fn_JRW() since now Petrale.Results.Dpth is included in Image.RData ####
}

# Make sure DateFile exists and is correct
DateFile

lib(VAST)
lib(SpatialDeltaGLMM)

(Year_Set = seq(min(DatG$Year),max(DatG$Year)))
(Years2Include = which( Year_Set %in% sort(unique(DatG$Year))))



str(Spatial_List$PolygonList$NN_Extrap)
# List of 2
# List of 2
#  $ nn.idx  : int [1:461606, 1] 246 246 246 246 246 246 246 246 246 246 ...
#  $ nn.dists: num [1:461606, 1] 594 592 590 589 587 ...

dim(cbind(Spatial_List$PolygonList$NN_Extrap$nn.idx, Spatial_List$PolygonList$NN_Extrap$nn.dists))
cbind(Spatial_List$PolygonList$NN_Extrap$nn.idx, Spatial_List$PolygonList$NN_Extrap$nn.dists)[1:10,]
#      [,1]     [,2]
# [1,]  246 593.7397
# [2,]  246 592.0681
# [3,]  246 590.3985
# [4,]  246 588.7310
# [5,]  246 587.0655
# [6,]  246 585.4021
# [7,]  246 583.7409
# [8,]  246 582.0817
# [9,]  246 580.4247
# [10,]  246 578.7699

Table(Spatial_List$PolygonList$NN_Extrap$nn.idx)


# Run PlotResultsOnMap_Fn_JRW to save the underlying grid data

# ******* MapDetails_List is now in Image.RData ********** 
# MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, Extrapolation_List = Extrapolation_List )

# source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Saves\\PlotMap_Fn_JRW - saved objects.R") # Gives an error when saving objects to global inside of a function

if(!exists("Petrale.Results.Dpth")) {
   
   source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\3 - Petrale Spatial Results\\Saves\\PlotMap_Fn_JRW.R")
   source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\3 - Petrale Spatial Results\\Saves\\PlotResultsOnMap_Fn_JRW.R")
   Petrale.Results.Dpth <- PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                               MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                               Year_Set=Year_Set, Years2Include = Years2Include, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                               cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(6, 6))
  gof()
}

# ------- Add an area label - counting from South to North on first latitude in the area (didn't bother with centroid) ----------
Petrale.X2003 <- sort.f(Petrale.Results.Dpth[!duplicated(Petrale.Results.Dpth$X2003), 3:5], 2)
Petrale.X2003$Area <- paste0("A:", 1:length(unique(Petrale.Results.Dpth$X2003))) 
# Petrale.Results.Dpth.Area <- match.f(Petrale.Results.Dpth[,-(1:2)], Petrale.X2003, "X2003", "X2003", "Area", round.=F) # Removed northings and eastings here
Petrale.Results.Dpth.Area <- match.f(Petrale.Results.Dpth, Petrale.X2003, "X2003", "X2003", "Area", round.=F) # Northings and eastings already gone

# Stack the data by year so all years can be plotted
# First year
Petrale.Results.Biomass.Stacked.Dpth <- data.frame(Petrale.Results.Dpth.Area[, c(1:3, ncol(Petrale.Results.Dpth.Area))], Year =  min(DatG$Year))
names(Petrale.Results.Biomass.Stacked.Dpth)[3] <- "CPUE.kgPerh"  #  Labeled here as not in log space - moved out of log space below
print(Petrale.Results.Biomass.Stacked.Dpth[1:2,])

# Second => last year
for ( i in 4:(ncol(Petrale.Results.Dpth.Area) - 1)) {
      cat("\n\n", i +  min(DatG$Year) - 3, "\n")
      Out <- data.frame(Petrale.Results.Dpth.Area[,c(1:2, i, ncol(Petrale.Results.Dpth.Area))], Year = i + min(DatG$Year) - 3)
      names(Out)[3] <- "CPUE.kgPerh"
      print(Out[1:2, ]); flush.console()  
      Petrale.Results.Biomass.Stacked.Dpth <- rbind(Petrale.Results.Biomass.Stacked.Dpth, Out)
}

# Moved out of log space
Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh <- exp(Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh)

dev.new()
histogram(~Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh | factor(Petrale.Results.Biomass.Stacked.Dpth$Year), type = 'count') # Without logging
dev.new()
histogram(~log(Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh) | factor(Petrale.Results.Biomass.Stacked.Dpth$Year), type = 'count') # Logged

# save(Petrale.Results.Biomass.Stacked.Dpth, file = "Petrale.Results.Biomass.Stacked.Dpth.RData")



# Find the quantiles 
tmp <- quantile(unique(Petrale.Results.Dpth.Area$X2003), seq(0, 1, length = length(unique(Petrale.Results.Dpth.Area$Area))))
(Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100))[1:10,]

# First year
Petrale.Quant.Biomass.Dpth <- renum(match.f(Petrale.Results.Dpth.Area[,c("X", "Y", "Area", "X2003")], Quant.Tab, "X2003", "Val", "Quant", round. = T, digits = 7))[,-4]
colnames(Petrale.Quant.Biomass.Dpth)[4] <- paste0("X", min(DatG$Year))

# Second => last year
for( i in 4:(ncol(Petrale.Results.Dpth.Area) - 1)) {
   bar(i, (ncol(Petrale.Results.Dpth.Area) - 1))
   tmp <- quantile(unique(Petrale.Results.Dpth.Area[,i]), seq(0, 1, length = length(unique(Petrale.Results.Dpth.Area$Area))))
   Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100)
   Temp <- match.f(Petrale.Results.Dpth.Area[,c(1, 2, i)], Quant.Tab, paste0("X", i + min(DatG$Year) - 3), "Val", "Quant", round. = T, digits = 7)
   Petrale.Quant.Biomass.Dpth <- cbind(Petrale.Quant.Biomass.Dpth, Temp[,4])
   colnames(Petrale.Quant.Biomass.Dpth)[ncol(Petrale.Quant.Biomass.Dpth)] <- paste0("X", i + min(DatG$Year) - 3)
}

sum(is.na(Petrale.Quant.Biomass.Dpth)) # No NA's now with using "round. = T, digits = 7"
Petrale.Quant.Biomass.Dpth[1:4,]

# save(Petrale.Quant.Biomass.Dpth, file = "Petrale.Quant.Biomass.Dpth.RData")


# Stack the quants
Petrale.Quant.Biomass.Stacked.Dpth <- data.frame(Petrale.Quant.Biomass.Dpth[,1:4], Year = 2003)
names(Petrale.Quant.Biomass.Stacked.Dpth)[4] <- "Quants" 

for ( i in 5:(ncol(Petrale.Quant.Biomass.Dpth))) {

      Out <- data.frame(Petrale.Quant.Biomass.Dpth[,c(1:3,i)], Year = i + 1999)
      names(Out)[4] <- "Quants" 

      Petrale.Quant.Biomass.Stacked.Dpth <- rbind(Petrale.Quant.Biomass.Stacked.Dpth, Out)
}

Petrale.Quant.Biomass.Stacked.Dpth <- renum(Petrale.Quant.Biomass.Stacked.Dpth)
sum(is.na(Petrale.Quant.Biomass.Stacked.Dpth)) # 0

# save(Petrale.Quant.Biomass.Stacked.Dpth, file="Petrale.Quant.Biomass.Stacked.Dpth.RData")  

  
# Find areas that are in the top 95% in 100% of the years: Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth 
 
Pet.Quant.80 <- Petrale.Quant.Biomass.Stacked.Dpth[Petrale.Quant.Biomass.Stacked.Dpth$Quants > 0.85, ]
Table(Pet.Quant.80$Area, Pet.Quant.80$Year)
dim(Table(Pet.Quant.80$Area, Pet.Quant.80$Year))

tmp <- sort(apply(Table(Pet.Quant.80$Area, Pet.Quant.80$Year), 1, function(x) sum(x > 0)))
Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth <- names(tmp[tmp >= 3])  
tmp2 <- Pet.Quant.80[Pet.Quant.80$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth,]
# Table(tmp2$Area, tmp2$Year)
dim(Table(tmp2$Area, tmp2$Year))

Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth
length(Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth)
# save(Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth, file= "Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.RData")

    
# ======================= Plot against Raw Data in DatG!!!!!!!!!!!!!!!!!!!!!!!!!!! =====================================

dev.new(); hist(Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh)

dev.new(); hist(DatG$Total_sp_wt_LR_kg/DatG$Area_Swept_ha)


# Col = colorRampPalette(colors = c("darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red"))
Col = colorRampPalette(colors = c("darkblue", "blue",  "orange", "red"))

      # AREAS <- unique(Petrale.Results.Biomass.Stacked.Dpth$Area[Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh > 3.1])   
      # Mean.Kg.per.Area.over.Years <- aggregate(list(Mean.CPUE.kgPerh = Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh), list(Area = Petrale.Results.Biomass.Stacked.Dpth$Area), mean)
      # Mean.Kg.per.Area.over.Years[Mean.Kg.per.Area.over.Years$Mean.CPUE.kgPerh > 2.800, ]
      # AREAS <- Mean.Kg.per.Area.over.Years$Area[Mean.Kg.per.Area.over.Years$Mean.CPUE.kgPerh > 2.800]
 
 
# ----- All n_x areas ------

# UTM.to.LatLong <- function(UTM, zone = "10T") {
#        lib(sp,  require = FALSE)
#         sputm <- sp::SpatialPoints(UTM, proj4string=CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84")))
#         spgeo <- sp::spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
#         OUT <- data.frame(spgeo@coords)
#		 names(OUT) <- c('Long', 'Lat')
#		 OUT
# }  

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
(PNG <- c(T, F)[1]) # TRUE = PNG; FALSE = Windows
(PLOTTER <- c("GIS", "Imap")[1])
(BIOMASS <- c(T, F)[2]) # TRUE  = Biomass Level; FALSE = Random color
(AREA.TYPE <- c("All", "Top")[2])
(DRAW.TRIANGULATION.MESH <- c(T, F)[2])
(SCALE.SIZE = 0.0025) # Scale for bubbles in the bubble plots
(CONTOUR <- c(T, F)[1])
(POLYGONS <- c(T, F)[1])
(PLOTLOWAREAS <- c(T, F)[2])

if(POLYGONS & !exists('AreaGroup'))
  load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\4 - Run VAST on WCGBTS\\Petrale WCGBTS AreaGroup 05 Apr 2018 3_25PM.RData")


if(.Platform$OS.type == "windows" & PNG) {
   (Dir <- paste0("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/R/Figs/", "Region = ", ifelse(Region == "Other", "Other", "CA Curr"), ", ", AREA.TYPE, " Areas, ", ifelse(BIOMASS, "Biomass Level", "Random Col"), "/"))
    dir.create(Dir, recursive=T)
} else Dir <- ""

# ---- Average over years of logged CPUE.kgPerh by Area ----
# hist(log(Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh))
# AverageLogMtByArea <- aggregate(list(AverageLogMt = Petrale.Results.Biomass.Stacked.Dpth$CPUE.kgPerh), list(Area = Petrale.Results.Biomass.Stacked.Dpth$Area), function(x) { round(2 * mean(log(x))) + 14 } )
# (AREAS <- AverageLogMtByArea$Area[AverageLogMtByArea$AverageLogMt %in% 11:12])  # exp((11 - 14)/2) = 0.22313 # so mean log of biomass > 0.22313
# Dir <- "Figs/Average over years of logged CPUE.kgPerh by Area/"


# ---- All Areas ----
if(AREA.TYPE == "All")
    (AREAS <- unique(Petrale.Results.Biomass.Stacked.Dpth$Area))  
 
# ---- Areas that are in the top 80% in at least 75% of the years ----
if(AREA.TYPE == "Top") 
    AREAS <- Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth
   
(N <- length(AREAS))


# GIS Test
# plotGIS(long = c(-125.1, -125), lat = c(47.8, 47.9))

# Coastal Areas
cA <- list()
cA[[1]] <- list(Name = "1. WA North", long = c(-126, -124), lat = c(47.25, 48.5))
cA[[2]] <- list(Name = "2. WA South & OR North", long = c(-126, -124), lat = c(44.9, 47.25))
cA[[3]] <- list(Name = "3. OR North & Central", long = c(-126, -124), lat = c(43.6, 46.0))
# cA[[4]] <- list(Name = "OR Central", long = c(-126, -124), lat = c(42.5, 45.15))
cA[[4]] <- list(Name = "4. OR Central & South", long = c(-126, -124), lat = c(42.3, 43.6))
cA[[5]] <- list(Name = "5. OR South, CA North", long = c(-126, -124), lat = c(40, 42.3))


for ( i in 1:5 ) {
#  for ( i in 4 ) {
     if(PNG) png(paste0(Dir, cA[[i]]$Name, ".png"), width = 2048, height = 2048, bg = 'grey')  else dev.new(width = 40, height = 30)
     plotGIS(long = cA[[i]]$long, lat = cA[[i]]$lat, levels.contour = { if(CONTOUR) { c(-60, -80, -100, -120, -140, seq(0, -2000, by = -200)) } else NULL })
     plot.bubble.zero.cross(DatG[, c("Longitude_dd", "Latitude_dd", "Total_sp_wt_LR_kg")], add=T, scale.size = SCALE.SIZE, cross.cex = ifelse(PNG, 1, 0.2), 
               cross.col = ifelse(i == 1, 'darkcyan', 'cyan') , fill.col='cyan', border.col='cyan', fill.col.alpha = 0.75, legend = F)
     for ( i in 1:N ) {
        DATA <- Petrale.Results.Biomass.Stacked.Dpth[Petrale.Results.Biomass.Stacked.Dpth$Area %in% AREAS[i],]
        if(BIOMASS) {
            # points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$CPUE.kgPerh))) + 14], pch = 19, cex=1.75)
     	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$CPUE.kgPerh))) + 14], pch = 19, cex = 1.6)
     	   points(DATA[, c("X","Y")], col = c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 1)], pch = sample(c(15,17,19),1), cex = 0.6)
         } else {
            Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14,2)]
            points(DATA[, c("X","Y")], col = Col.rnd[1], pch = sample(c(15,17,19),1), cex = ifelse(PNG, 1.6, 0.7))
     	    points(DATA[, c("X","Y")], col = Col.rnd[2], pch = sample(c(15,17,19),1), cex = ifelse(PNG, 0.6, 0.2))
        }
     }
	 if(PLOTLOWAREAS) {
	     lowAREAS <- unique(Petrale.Quant.Biomass.Stacked.Dpth$Area[!Petrale.Quant.Biomass.Stacked.Dpth$Area %in% AREAS])
		 lowN <- length(lowAREAS)
		  for ( i in 1:lowN ) {
		    Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14,1)]
            DATA <- Petrale.Results.Biomass.Stacked.Dpth[Petrale.Results.Biomass.Stacked.Dpth$Area %in% lowAREAS[i],]
            points(DATA[, c("X","Y")], col = Col.rnd, pch = 15, cex = 0.7)
		 }
     }
	 # plot.bubble.zero.cross(DatG[, c("Longitude_dd", "Latitude_dd", "Total_sp_wt_LR_kg")], add=T, scale.size = SCALE.SIZE, cross.cex = ifelse(PNG, 1, 0.2), 
     #           fill.col='cyan', border.col='cyan', fill.col.alpha = 0.75, legend = F)
     if(Region == "Other" & BIOMASS & POLYGONS)  for( i in 1:10)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.75)
     if(DRAW.TRIANGULATION.MESH) {
         SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
         SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
         plot(SpatialIsoLL, draw.vertices = T, lwd = 2, size = 10, add=T)
     	 points(UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=2)  # Add larger vertices (knots)
     }
	 if(POLYGONS) {
        for (G in 1:10)
             polygon(AreaGroup[[G]]$Boundary, col=col.alpha('purple', 0.25))
     }	
	 if(PNG) gof()
}


#####  Figure for paper  ##########

long <- c(-125.7, -123.6); lat <- c(43.75, 45.85)

if(PNG) png(paste0(Dir, "3. OR North & Central", ".png"), width = 775, height = 1024, bg = 'grey')  else dev.new(width = 40, height = 30)
plotGIS(long = long, lat = lat, levels.contour = { if(CONTOUR) { c(-60, -80, -100, -120, -140, seq(-200, -2000, by = -200)) } else NULL }, Fname = 'TMP.tif')   ############
plot.bubble.zero.cross(DatG[DatG$Total_sp_wt_LR_kg > 0, c("Longitude_dd", "Latitude_dd", "Total_sp_wt_LR_kg")], add=T, scale.size = SCALE.SIZE, cross.cex = ifelse(PNG, 1, 0.2), 
          cross.col = ifelse(i == 1, 'darkcyan', 'cyan') , fill.col='deepskyblue3', border.col='deepskyblue3', fill.col.alpha = 0.75, legend = FALSE)
if(Region == "Other" & POLYGONS)  for( i in 1:10)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.0)
if(POLYGONS) {
   for (G in 1:2)
        polygon(AreaGroup[[G]]$Boundary, col=col.alpha('purple', 0.20))
}	
     
text(-123.825, 45.77, "Cape Falcon", col = 'aquamarine')
text(-123.87, 44.62, "Yaquina Bay", col = 'aquamarine') 

rect(-125.6, 44.75, -124.9, 45.65, col='grey91')

TeachingDemos::subplot( { par(ps = 10); plot(c(-126, -116), c(31, 49), xlab = "", ylab = "", axes = FALSE); 
                   axis(2, at = c(35, 40, 45), labels = c("35N", "40N", "46N"), col.axis = 'black', cex.axis = 1.4, font.axis = 2); 
                   axis(1, at = c(-126, -122, -118), labels = c("126W", "122W", "118W"), col.axis = 'black', cex.axis = 1.4, font.axis = 2);
                   rect(-127, 30, -115, 50, lwd = 0, col = 'grey81'); lines(world.h.land); box(); polygon(world.h.land, col = 'grey40', border = NA); 
                   lines(world.h.borders, col = "cyan"); rect(long[1], lat[1], long[2], lat[2], lwd = 2, border = 'red');
                   text(c(-119.96, -120.90, -122.16), c(47.82, 44.67, 40.74), c('WA', 'OR', 'CA'), cex = 1.2) }, 
                   x=grconvertX(c(0.09, 0.37), from='npc'), y=grconvertY(c(0.5, 0.9), from='npc'), type='fig', pars=list( mar=c(1,1,1,1) + 0.1) )    
                   
if(PNG) gof()




   
   
if(F) {

    # Entire Coast Polygons Only
    if(PNG) png(paste0(Dir, "Entire Coast Polygons Only.png"), width = 2048, height = 2048, bg = 'grey')  else  dev.new()
    imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
    for ( i in 1:length(AreaGroup))
         polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple'))
    if(PNG) gof()
    
    # Entire Coast Polygons Only - Colored by biomass increase 
    if(PNG) png(paste0(Dir, "Entire Coast Polygons Only GIS.png"), width = 2048, height = 2048, bg = 'grey')  else  dev.new()
    # imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
    plotGIS(long = c(-126, -123.6), lat = c(39, 48.5), levels.contour = seq(0, -600, by = -200))
    for ( i in 1:length(AreaGroup))
         polygon(AreaGroup[[i]]$Boundary, col=col.alpha(c('purple', 'purple', 'blue', 'purple', 'red', 'purple',
    	                                    'blue', 'red', 'red', 'blue', 'blue')[i],1))
    if(PNG) gof()
    
    
    # Entire coast - no raw data bubble plots
    if(PNG) png(paste0(Dir, "Entire Coast, No Data, All Areas.png"), width = 2048, height = 2048, bg = 'grey')  else  dev.new()
    imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
    for ( i in 1:length(AreaGroup))
         polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.25))
    for ( i in 1:N ) {
       DATA <- Petrale.Results.Biomass.Stacked.Dpth[Petrale.Results.Biomass.Stacked.Dpth$Area %in% AREAS[i], ]
       # cat("\n", c(mean(log(DATA$CPUE.kgPerh)), round(2 * mean(log(DATA$CPUE.kgPerh))) + 14), "\n")
       if(BIOMASS) 
           points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$CPUE.kgPerh))) + 14], pch = 16, cex=0.8)
       else 	   
           points(DATA[, c("X","Y")], col = heat.colors(n = 50)[sample(50,1)] , pch = sample(0:25, 1), pch = 16, cex=0.8)
    }
    if(PNG) gof()
    
    # Entire Coast with bubble plots
    if(PNG) png(paste0(Dir, "Entire Coast.png"), width = 2048, height = 2048, bg = 'grey')  else  dev.new()
    # imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
    imap(longrange = c(-128, -118), latrange = c(32,49.5), zoom = F) # For triangulation mesh
    plot.bubble.zero.cross(DatG[, c("Longitude_dd", "Latitude_dd", "Total_sp_wt_LR_kg")], add=T, scale.size = 0.0007, fill.col='cyan', border.col='cyan', cross.cex = 0)
    # for ( i in 1:length(AreaGroup))
    #    polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.5))
    for ( i in 1:N ) {
       DATA <- Petrale.Results.Biomass.Stacked.Dpth[Petrale.Results.Biomass.Stacked.Dpth$Area %in% AREAS[i], ]
       # cat("\n", c(mean(log(DATA$CPUE.kgPerh)), round(2 * mean(log(DATA$CPUE.kgPerh))) + 14), "\n")
       DATA$X <- DATA$X - 2
       if(BIOMASS) {
    	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$CPUE.kgPerh))) + 14], pch = 16, cex=0.8)
       } else 	   
           points(DATA[, c("X","Y")], col = heat.colors(n = 50)[sample(50,1)] , pch = sample(0:25, 1), cex=0.8)
    }
    if(DRAW.TRIANGULATION.MESH) {
        SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
        SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
        plot(SpatialIsoLL, draw.vertices = T, lwd = 2, size = 10, add=T)
    	points(UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=2)  # Add larger vertices (knots)
    }
    if(PNG) gof()
    
    
    # Entire Coast with only bubble plots and triangulation mesh
    if(PNG) png(paste0(Dir, "Entire Coast, Data, Mesh.png"), width = 2048, height = 2048, bg = 'grey')  else  dev.new()
    imap(longrange = c(-128, -118), latrange = c(32,49.5), zoom = F) # For triangulation mesh
    plot.bubble.zero.cross(DatG[, c("Longitude_dd", "Latitude_dd", "Total_sp_wt_LR_kg")], add=T, scale.size = 0.0007, fill.col='cyan', border.col='cyan')
    
    SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
    SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
    plot(SpatialIsoLL, draw.vertices = T, add=T)
    
    if(PNG) gof()
    
    # points(DATA[, c("X","Y")], col = heat.colors(n = 50)[sample(50,1)] , pch = sample(0:25, 1), cex = 3) 
    
}


   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   