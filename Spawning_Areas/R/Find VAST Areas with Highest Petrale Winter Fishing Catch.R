

###################################################################################
###         Using:                                                                                                                                                                                   ###
###                R 3.5.1                                                                                                                                                                           ###
###                FishStatsUtils ver 2.0.0      (A Jim Thorson's package on GitHub that is used with VAST .)                  ###
###                Imap built under R 4.3.1 (manually copied  from R 4.3.1 library folder to  R 3.5.1 library folder)    ###
###################################################################################

#  R package achive: https://cran.r-project.org/src/contrib/Archive
# install.packages("https://cran.r-project.org/src/contrib/Archive/remotes/remotes_2.1.0.tar.gz", repos = NULL)


setwd("Petrale Spatial Results") # Change all paths as needed


if (!any(installed.packages()[, 1] %in% "JRWToolBox")) {
       if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes') 
       remotes::install_github("John-R-Wallace-NOAA/JRWToolBox")
}

library(JRWToolBox)
lib(remotes) # JRWToolBox::lib() will both install (only when needed) and load the package. 
lib("John-R-Wallace-NOAA/Imap") 

# Not all of these tar ball installs (which need compiling) are working for me with Windows  R ver 3.5.1, hence *** ONLY MY LEGACY COMPLIED VERSIONS HAVE ALLOWED ME TO MAKE THIS WORK ***
install.packages("https://cran.r-project.org/src/contrib/Archive/maps/maps_3.3.0.tar.gz", repos = NULL) 
remotes::install_version('lattice', '0.20-41', repos = "http://cran.us.r-project.org")
remotes::install_version('mapdata', '2.3.0', repos = "http://cran.us.r-project.org")
remotes::install_version('sp', '1.4-2', repos = "http://cran.us.r-project.org")
remotes::install_version('maptools', '0.9-9', repos = "http://cran.us.r-project.org")


lib("John-R-Wallace-NOAA/Imap")
lib('nwfsc-assess/geostatistical_delta-GLMM', Package.Name = 'SpatialDeltaGLMM', q = FALSE)

Source("PlotMap_Fn_JRW.R") # In the same GitHub folder as this R script
Source("PlotResultsOnMap_Fn_JRW.R") # In the same GitHub folder as this R script


# -----------------------------------------------------------------------------------------------------------------------------------------

# Pick the region used in VAST
(Region <- c("Other", "California_current")[1]) # Only region  "Other" was found to work with this approach. See the VAST help for information on regions.
 

# Use base::load()  to avoid my  JRWToolBox::load(), which loads twice.

# --------- CA Current Region -----------
if(Region == "California_current")	{ 
    # 300 knots
    base::load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass n_x = 300/2018-01-05_Petrale_CW_NOCV_Winter_CA_Curr_nx=300/Image.RData")  # More restricted data
    DateFile <- "W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass n_x = 300/2018-01-05_Petrale_CW_NOCV_Winter_CA_Curr_nx=300/"
}
    
# ----------- "Other" Region ---------
if(Region == "Other")	  {
    # 100 Knots
    # base::load("W:\\ALL_USR\\JRW\\R.Vanilla\\2018-01-03_Petrale_CW_NOCV_WinterOther Reg_nx=100\\Image.Rdata")
    # DateFile <- "W:/ALL_USR/JRW/R.Vanilla/2018-01-03_Petrale_CW_NOCV_Winter_Other Reg_nx=100/" 
    
    # 300 Knots
    # base::load("\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\Petrale CW NOCV Biomass, Other Reg\\2018-01-03_Petrale_CW_NOCV_Winter_Other Reg_nx=300\\Image.RData") 
    # DateFile <- "\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\Petrale CW NOCV Biomass, Other Reg\\2018-01-03_Petrale_CW_NOCV_Winter_Other Reg_nx=300\\"
    DateFile <- "W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass n_x = 300/2018-01-03_Petrale_Winter_Other/" 
    # Make sure DateFile exists and is correct
    opendir(DateFile)
    base::load(paste0(DateFile, "Image.RData")) 
         
}

(Year_Set = seq(min(DatG$RYEAR),max(DatG$RYEAR)))
# Years2Include = which( Year_Set %in% sort(unique(DatG$RYEAR)))


str(Spatial_List$PolygonList$NN_Extrap)
List of 2
#  $ nn.idx  : int [1:45628, 1] 21 21 21 21 21 21 21 21 21 21 ...
#  $ nn.dists: num [1:45628, 1] 216 216 216 216 216 ...

cbind(Spatial_List$PolygonList$NN_Extrap$nn.idx, Spatial_List$PolygonList$NN_Extrap$nn.dists)[1:10,]
#       [,1]     [,2]
#  [1,]   21 216.0902
#  [2,]   21 216.0558
#  [3,]   21 216.0400
#  [4,]   21 216.0427
#  [5,]   21 216.0640
#  [6,]   21 216.1038
#  [7,]   21 218.5918
#  [8,]   21 218.9057
#  [9,]   21 219.2374
# [10,]   21 219.5868


# Run PlotResultsOnMap_Fn_JRW to save the underlying grid data

# Create Petrale.Results.Dpth.Rst if needed
if(TRUE) {
    # ******* MapDetails_List is not in Image.RData - it has to recreated here ********** !!
    MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, Extrapolation_List = Extrapolation_List )
    
    # source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Saves\\PlotMap_Fn_JRW - saved objects.R") # Gives an error when saving objects to global inside of a function
    
    Petrale.Results.Dpth.Rst <- PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                                   MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                                   Year_Set=Year_Set, Years2Include= 1981:2015 - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                                   cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(6, 6))
    gof()
}

# Petrale.Results.Dpth.Rst.SAVE <- Petrale.Results.Dpth.Rst


# ------- Add an area label - counting from south to north on first latitude in the area (didn't bother with centroid) ----------
Petrale.X1981 <- sort.f(Petrale.Results.Dpth.Rst[!duplicated(Petrale.Results.Dpth.Rst$X1981), 3:5], 2)
Petrale.X1981$Area <- paste0("A:", 1:length(unique(Petrale.Results.Dpth.Rst$X1981))) # 300 knots for now
Petrale.Results.Dpth.Rst <- match.f(Petrale.Results.Dpth.Rst[,-(1:2)], Petrale.X1981, "X1981", "X1981", "Area", round.=F) # Removed northings and eastings here


# Stack the data by year so all years can be plotted, moved out of log space, and converted to metric tons from lbs
Petrale.Results.Biomass.Stacked.Dpth.Rst <- data.frame(Petrale.Results.Dpth.Rst[, c(1:3, ncol(Petrale.Results.Dpth.Rst))], Year = 1981)
names(Petrale.Results.Biomass.Stacked.Dpth.Rst)[3] <- "Biomass.mt"  #  Name with units here - converted after stacking

# for ( i in 4:10) {
for ( i in 4:(ncol(Petrale.Results.Dpth.Rst) - 1)) {
      cat("\n\n", i + 1978, "\n")
      Out <- data.frame(Petrale.Results.Dpth.Rst[,c(1:2, i, ncol(Petrale.Results.Dpth.Rst))], Year = i + 1978)
      names(Out)[3] <- "Biomass.mt"
      print(Out[1:2, ]); flush.console()  
      Petrale.Results.Biomass.Stacked.Dpth.Rst <- rbind(Petrale.Results.Biomass.Stacked.Dpth.Rst, Out)
}

# lbs to mt; labeled as mt above
Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt <- exp(Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt)/2.20462/1000

histogram(~Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt | factor(Petrale.Results.Biomass.Stacked.Dpth.Rst$Year), type = 'count') # Without logging
windows()
histogram(~log(Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt) | factor(Petrale.Results.Biomass.Stacked.Dpth.Rst$Year), type = 'count') # Logged

save(Petrale.Results.Dpth.Rst, 
 file = "W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass Regions n_x = 300/2018-01-03_Petrale_CW_NOCV_Winter_Other_nx=300/Petrale.Results.Dpth.Rst.RData")
save(Petrale.Results.Biomass.Stacked.Dpth.Rst, 
 file = "W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass Regions n_x = 300/2018-01-03_Petrale_CW_NOCV_Winter_Other_nx=300/Petrale.Results.Biomass.Stacked.Dpth.Rst.RData")

 
 #####  Find the quantiles  #####

# load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\2 - Run VAST Model\\Petrale CW NOCV Biomass Regions n_x = 300\\Petrale.Results.Dpth.Rst.RData")
# load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\2 - Run VAST Model\\Petrale CW NOCV Biomass Regions n_x = 300\\Petrale.Results.Biomass.Stacked.Dpth.Rst.RData") 

# Copy of files here: 
# load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass Regions n_x = 300/2018-01-03_Petrale_CW_NOCV_Winter_Other_nx=300/Petrale.Results.Dpth.Rst.RData") 
# load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/2 - Run VAST Model/Petrale CW NOCV Biomass Regions n_x = 300/2018-01-03_Petrale_CW_NOCV_Winter_Other_nx=300/Petrale.Results.Biomass.Stacked.Dpth.Rst.RData") 


tmp <- quantile(unique(Petrale.Results.Dpth.Rst$X1981), seq(0, 1, length = length(unique(Petrale.Results.Dpth.Rst$Area))))
(Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100))[1:10,]

Petrale.Quant.Biomass.Dpth.Rst <- renum(match.f(Petrale.Results.Dpth.Rst[,c("X", "Y", "Area", "X1981")], Quant.Tab, "X1981", "Val", "Quant", round. = T, digits = 7))[,-4]
colnames(Petrale.Quant.Biomass.Dpth.Rst)[4] <- paste0("X", 1981)

for( i in 4:37) {
   bar(i, 37)
   tmp <- quantile(unique(Petrale.Results.Dpth.Rst[,i]), seq(0, 1, length = length(unique(Petrale.Results.Dpth.Rst$Area))))
   Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100)
   Temp <- match.f(Petrale.Results.Dpth.Rst[,c(1, 2, i)], Quant.Tab, paste0("X", i + 1978), "Val", "Quant", round. = T, digits = 7)
   Petrale.Quant.Biomass.Dpth.Rst <- cbind(Petrale.Quant.Biomass.Dpth.Rst, Temp[,4])
   colnames(Petrale.Quant.Biomass.Dpth.Rst)[ncol(Petrale.Quant.Biomass.Dpth.Rst)] <- paste0("X", i + 1978)
}

sum(is.na(Petrale.Quant.Biomass.Dpth.Rst)) # No NA's now with using "round. = T, digits = 7"
Petrale.Quant.Biomass.Dpth.Rst[1:4,]

# save(Petrale.Quant.Biomass.Dpth.Rst, file = "Petrale.Quant.Biomass.Dpth.Rst.RData")


# Stack the quants
Petrale.Quant.Biomass.Stacked.Dpth.Rst <- data.frame(Petrale.Quant.Biomass.Dpth.Rst[,1:4], Year = 1981)
names(Petrale.Quant.Biomass.Stacked.Dpth.Rst)[4] <- "Quants" 

for ( i in 5:(ncol(Petrale.Quant.Biomass.Dpth.Rst))) {

      Out <- data.frame(Petrale.Quant.Biomass.Dpth.Rst[,c(1:3,i)], Year = i + 1977)
      names(Out)[4] <- "Quants" 

      Petrale.Quant.Biomass.Stacked.Dpth.Rst <- rbind(Petrale.Quant.Biomass.Stacked.Dpth.Rst, Out)
}

Petrale.Quant.Biomass.Stacked.Dpth.Rst <- renum(Petrale.Quant.Biomass.Stacked.Dpth.Rst)

sum(is.na(Petrale.Quant.Biomass.Stacked.Dpth.Rst)) # 0

# save(Petrale.Quant.Biomass.Stacked.Dpth.Rst, file="Petrale.Quant.Biomass.Stacked.Dpth.Rst.RData")  
  
# Find areas that are in the top 20% in at least 40% of the years: Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst 
 
Pet.Quant.80 <- Petrale.Quant.Biomass.Stacked.Dpth.Rst[Petrale.Quant.Biomass.Stacked.Dpth.Rst$Quants > 0.80, ]
# Table(Pet.Quant.80$Area, Pet.Quant.80$Year)[1:10,]
dim(Table(Pet.Quant.80$Area, Pet.Quant.80$Year))

tmp <- sort(apply(Table(Pet.Quant.80$Area, Pet.Quant.80$Year), 1, function(x) sum(x > 0)))
Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst <- names(tmp[tmp >= 0.40 * 35])  # 14/35 = 0.40
tmp2 <- Pet.Quant.80[Pet.Quant.80$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst,]
# Table(tmp2$Area, tmp2$Year)
dim(Table(tmp2$Area, tmp2$Year))

Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst
len(Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst)

save(Petrale.Results.Dpth.Rst, file = "Petrale.Results.Dpth.Rst.RData")
save(Petrale.Results.Biomass.Stacked.Dpth.Rst, file = "Petrale.Results.Biomass.Stacked.Dpth.Rst.RData")
save(Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst, file = "Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst.RData")
    
# ======================= Plot against Raw Data in DatG!!=====================================

# Load New DatG with more depth restrictions
# load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\2 - Run VAST Model\\Petrale CW NOCV Biomass Regions n_x = 300\\Petrale.Results.Dpth.Rst.RData")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\2 - Run VAST Model\\Petrale CW NOCV Biomass n_x = 300\\Petrale.Results.Dpth.Rst.RData")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\2 - Run VAST Model\\Petrale CW NOCV Biomass n_x = 300\\Petrale.Results.Biomass.Stacked.Dpth.Rst.RData")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\2 - Run VAST Model\\Petrale CW NOCV Biomass n_x = 300\\Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst.RData")

# Load polygon area groups for PacFIN Petrale Winter Fishery
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale AreaGroup 10 Jan 2018 4_48PM.RData")


# windows()
# hist(Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt)

# windows()
# hist(DatG$ptrlbs)

# DatG.SAVE <- DatG
# DatG <- DatG[(abs(DatG$DEPTH1*1.8288 - DatG$DepthGIS.m) %<=% 250 | is.na(DatG$DEPTH1)) & DatG$DepthGIS.m %<=% 1281,]


# Col = colorRampPalette(colors = c("darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red"))
Col = colorRampPalette(colors = c("darkblue", "blue",  "orange", "red"))

      # AREAS <- unique(Petrale.Results.Biomass.Stacked.Dpth.Rst$Area[Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt > 3.1])   
      # Mean.Kg.per.Area.over.Years <- aggregate(list(Mean.Biomass.mt = Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt), list(Area = Petrale.Results.Biomass.Stacked.Dpth.Rst$Area), mean)
      # Mean.Kg.per.Area.over.Years[Mean.Kg.per.Area.over.Years$Mean.Biomass.mt > 2.800, ]
      # AREAS <- Mean.Kg.per.Area.over.Years$Area[Mean.Kg.per.Area.over.Years$Mean.Biomass.mt > 2.800]
 
 
# ----- All 300 areas ------

UTM.to.LatLong <- function(UTM, zone = "10T") {
         lib(sp,  require = FALSE)
         sputm <- sp::SpatialPoints(UTM, proj4string=CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84")))
         spgeo <- sp::spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
         OUT <- data.frame(spgeo@coords)
		 names(OUT) <- c('Long', 'Lat')
		 OUT
}  

# -----------------------------------------------------------------------------------------------------------------
 
(PNG <- c(T, F)[1]) # TRUE = PNG; FALSE = Windows
(PLOTTER <- c("GIS", "Imap")[1])
(BIOMASS <- c(T, F)[1]) # TRUE  = Biomass Level; FALSE = Random color
(AREA.TYPE <- c("All", "Top")[2])
(DRAW.TRIANGULATION.MESH <- c(T, F)[2])

if(PNG) {
   (Dir <- paste0("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/R/Figs/", "Region = ", ifelse(Region == "Other", "Other", "CA Curr"), ", ", AREA.TYPE, " Areas, ", ifelse(BIOMASS, "Biomass Level", "Random Col"), "/"))
   dir.create(Dir, recursive=T)
   opendir(Dir)
}

# ---- Average over years of logged Biomass.mt by Area ----
# hist(log(Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt))
# AverageLogMtByArea <- aggregate(list(AverageLogMt = Petrale.Results.Biomass.Stacked.Dpth.Rst$Biomass.mt), list(Area = Petrale.Results.Biomass.Stacked.Dpth.Rst$Area), function(x) { round(2 * mean(log(x))) + 14 } )
# (AREAS <- AverageLogMtByArea$Area[AverageLogMtByArea$AverageLogMt %in% 11:12])  # exp((11 - 14)/2) = 0.22313 # so mean log of biomass > 0.22313
# Dir <- "Figs/Average over years of logged Biomass.mt by Area/"


# ---- All Areas ----
if(AREA.TYPE == "All")
    (AREAS <- unique(Petrale.Results.Biomass.Stacked.Dpth.Rst$Area))  
 
# ---- Areas that are in the top 80% in at least 75% of the years ----
if(AREA.TYPE == "Top")
    AREAS <- Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.Rst
   
(N <- length(AREAS)) # 65 is correct for Winter Petrale Fishery


# GIS Test
# plotRAST(long = c(-125.1, -125), lat = c(47.8, 47.9))

# Entire Coast Polygons Only
if(PNG) png(paste0(Dir, "Entire Coast Polygons Only.png"), width = 2048, height = 2048, bg = 'grey')  else  windows()
imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple'))
if(PNG) gof()

# Entire Coast Polygons Only - Colored by biomass increase 
if(PNG) png(paste0(Dir, "Entire Coast Polygons Only GIS.png"), width = 2048, height = 2048, bg = 'grey')  else  windows()
# imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
plotRAST(long = c(-126, -123.6), lat = c(39, 48.5), levels.contour = seq(0, -600, by = -200))
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha(c('purple', 'purple', 'blue', 'purple', 'red', 'purple',
	                                    'blue', 'red', 'red', 'blue', 'blue')[i],1))
if(PNG) gof()


# Entire coast - no raw data bubble plots
if(PNG) png(paste0(Dir, "Entire Coast, No Data, All Areas.png"), width = 2048, height = 2048, bg = 'grey')  else  windows()
imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.25))
for ( i in 1:N ) {
   DATA <- Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Area %in% AREAS[i], ]
   # cat("\n", c(mean(log(DATA$Biomass.mt)), round(2 * mean(log(DATA$Biomass.mt))) + 14), "\n")
   if(BIOMASS) 
       points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 16, cex=0.8)
   else 	   
       points(DATA[, c("X","Y")], col = heat.colors(n = 50)[sample(50,1)] , pch = sample(0:25, 1), pch = 16, cex=0.8)
}
if(PNG) gof()


# Entire Coast with bubble plots
if(PNG) png(paste0(Dir, "Entire Coast.png"), width = 2048, height = 2048, bg = 'grey')  else  windows()
# imap(longrange = c(-126, -120), latrange = c(32,48.5), zoom = F)
imap(longrange = c(-128, -118), latrange = c(32,49.5), zoom = F) # For triangulation mesh
plot.bubble.zero.cross(DatG[, c("SET_LONG", "SET_LAT", "ptrlbs")], add=T, scale.size = 0.0007, fill.col='cyan', border.col='cyan')
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.5))
for ( i in 1:N ) {
   DATA <- Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Area %in% AREAS[i], ]
   # cat("\n", c(mean(log(DATA$Biomass.mt)), round(2 * mean(log(DATA$Biomass.mt))) + 14), "\n")
   if(BIOMASS) {
	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 16, cex=0.8)
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
if(PNG) png(paste0(Dir, "Entire Coast, Data, Mesh.png"), width = 2048, height = 2048, bg = 'grey')  else  windows()
imap(longrange = c(-128, -118), latrange = c(32,49.5), zoom = F) # For triangulation mesh
plot.bubble.zero.cross(DatG[, c("SET_LONG", "SET_LAT", "ptrlbs")], add=T, scale.size = 0.0007, fill.col='cyan', border.col='cyan')

SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
plot(SpatialIsoLL, draw.vertices = T, add=T)

if(PNG) gof()

# points(DATA[, c("X","Y")], col = heat.colors(n = 50)[sample(50,1)] , pch = sample(0:25, 1), cex = 3) 


# North WA
if(PNG) png(paste0(Dir, "WA North.png"), width = 2048, height = 2048, bg = 'grey') else windows()
if(PLOTTER == "GIS") plotRAST(long = c(-126, -124), lat = c(47.25, 48.5)) else imap(longr = c(-126, -124), latr = c(47.25, 48.5), z=F)
plot.bubble.zero.cross(DatG[DatG$ptrlbs >= 0, c("SET_LONG", "SET_LAT", "ptrlbs")], add=T, scale.size = 0.007, fill.col='cyan', border.col='cyan')
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.5))
for ( i in 1:N ) {
   DATA <- Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Area %in% AREAS[i],]
   if(BIOMASS) {
       # points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex = 3)
	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex = 2.5)
	   points(DATA[, c("X","Y")], col = c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 1)], pch = sample(c(15,17,19),1), cex = 1)
   } else {
       Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 2)]
       points(DATA[, c("X","Y")], col = Col.rnd[1], pch = sample(c(15,17,19),1), cex = 2.5)
	   points(DATA[, c("X","Y")], col = Col.rnd[2], pch = sample(c(15,17,19),1), cex = 1)
   }
   # cat("\n", AREAS[i], round(2 * mean(log(DATA$Biomass.mt))) + 14, Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], "\n")
}
if(Region == "Other" & BIOMASS)  for( i in 1:11)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.75)
if(DRAW.TRIANGULATION.MESH) {
    SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
    SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
    plot(SpatialIsoLL, draw.vertices = T, lwd = 2, size = 10, add=T)
	points(UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=2)  # Add larger vertices (knots)
}
if(PNG) gof() 


 # WA South, OR North
if(PNG) png(paste0(Dir, "WA South, OR North.png"), width = 2048, height = 2048, bg = 'grey') else  windows()
plotRAST(long = c(-126, -124), lat = c(44.9, 47.25))
# imap(longrange = c(-126, -124), latrange = c(44.9, 47.25), zoom=FALSE)

# plot.bubble.zero.cross(DatG[DatG$ptrlbs >= 0, c("SET_LONG", "SET_LAT", "ptrlbs")], add=T, scale.size = 0.007, fill.col='cyan', border.col='cyan') 
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.5))
for ( i in 1:N ) {
   DATA <- Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Area %in% AREAS[i],]
   if(BIOMASS) {
       # points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex=1.75)
	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex = 1.6)
	   points(DATA[, c("X","Y")], col = c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 1)], pch = sample(c(15,17,19),1), cex = 0.6)
    } else {
       Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14,2)]
       points(DATA[, c("X","Y")], col = Col.rnd[1], pch = sample(c(15,17,19),1), cex = 1.6)
	   points(DATA[, c("X","Y")], col = Col.rnd[2], pch = sample(c(15,17,19),1), cex = 0.6)
   }
}
if(Region == "Other" & BIOMASS)  for( i in 1:11)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.75)
if(DRAW.TRIANGULATION.MESH) {
    SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
    SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
    plot(SpatialIsoLL, draw.vertices = T, lwd = 2, size = 10, add=T)
	points(UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=2)  # Add larger vertices (knots)
}
if(PNG) gof()


{
 # WA South, OR North ############## For paper ################

long <- c(-125.1, -123.8); lat <- c(44.9, 45.9) 

Source("W:\\ALL_USR\\JRW\\R.Vanilla\\plotRAST.R")

PNG <- c(TRUE, FALSE )[1]
 
# if(PNG) png(paste0(Dir, "WA South, OR North.png"), width = 1024, height = 1024, bg = 'grey') else  windows()
tiff(paste0(Dir, "WA South, OR North.tiff"), width = 1024, height = 1024, bg = 'grey')

#  dev.new(); plotRAST(long = long, lat = lat, col.bathy = rev(grDevices::terrain.colors(50))) # hcl.colors
# https://oceanexplorer.noaa.gov/edu/learning/player/lesson04/l4la1.htm

# plotRAST(long = long, lat = lat, col.bathy = rev(grDevices::terrain.colors(50)), levels.contour = seq(-100, -2000, by = -300)) 

# # Col.2 = colorRampPalette(colors = rev(c("firebrick3", "orange", "yellow", "green", "blue", "violet")))
# # plotRAST(long = long, lat = lat, col.bathy = Col.2(255), levels.contour = seq(-100, -2000, by = -100)) 
# # dev.new(); plotRAST(long = long, lat = lat, col.bathy = rev(rainbow(255)[1:200]), levels.contour = seq(-100, -2000, by = -100)) 

# Use maxElev arg and limit green color by using only the first 32 colors out of the 50.
# plotRAST(long = long, lat = lat, maxElev = 0, col.bathy = rev(grDevices::terrain.colors(50))[1:32], levels.contour = seq(-100, -2000, by = -300)) 

# Match colors of other figure in paper  #  Fname = "TMP.tif"
# Col.3 = colorRampPalette(colors = c(rgb(0/255, 57/255, 147/255), rgb(35/255, 78/255, 173/255), rgb(85/255, 109/255, 200/255), rgb(139/255, 147/255, 218/255), rgb(213/255, 217/255, 248/255)))
# plotRAST(long = long, lat = lat, maxElev = 0, col.bathy = Col.3(5), levels.contour = c(-50, -150, -400, -2600), col.imap = rgb(244/255, 198/255, 104/255), Fname = "TMP.tif") 

Col.fixed = c(rgb(0/256, 57/256, 147/256), rgb(35/256, 78/256, 173/256), rgb(85/256, 109/256, 200/256), rgb(139/256, 147/256, 218/256), rgb(213/255, 217/255, 248/255))

# The terra package calls the filled.contour() function which screws up the adding of its own contour lines or the imap overlay. A fudge factor had to used in plotRAST and the text below!!!!!!!!!!! See below for for all the attempts to make it right.
plotRAST(long = long, lat = lat, maxElev = 100, levels.contour = rev(c(80, -50, -150, -400, -2600)), filled = TRUE, col.contour = Col.fixed, col.imap = rgb(156/256, 156/256, 156/256)) 

# (df <-  raster::rasterToPoints(raster::raster(Fname)))[1:3, ]
# BathySmall.list <- wireframe.lattice.f(df[, 1], df[, 2], df[, 3], grid.len.x = 40)
# fitted.contour(BathySmall.list, levels = rev(c(0, -50, -150, -400, -2600)), col = Col.fixed)

# imap(longrange = long, latrange = lat, zoom = FALSE)

# center.points = TRUE for center points fig
plot.bubble.zero.cross(DatG[DatG$ptrlbs > 0 & DatG$SET_LONG > long[1] & DatG$SET_LONG < long[2] & DatG$SET_LAT > lat[1] & DatG$SET_LAT < lat[2], c("SET_LONG", "SET_LAT", "ptrlbs")], 
             add=T, scale.size = 0.05, fill.col = c('deepskyblue3', 'greenyellow')[2], fill.col.alpha = 0.2, border.col = c('deepskyblue3', 'greenyellow')[2], legend = FALSE, center.points = FALSE) # scale.size = 0.007 was in original fig
             
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha(c('purple', 'lightpink2')[2], 0.2))


if(Region == "Other" & BIOMASS)  for( i in 1:11)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex = 1.75)

text(-123.90 - 0.10, 45.77, "Cape Falcon", col = c('aquamarine', 'black')[2], cex = 1.4)
text(-123.8935 - 0.10, 45.213, "Haystack Rock", col = c('aquamarine', 'black')[2], cex = 1.4)
                 
TeachingDemos::subplot( { par(ps = 10); plot(c(-126, -116), c(31, 49), xlab = "", ylab = "", axes = FALSE); 
                   axis(2, at = c(35, 40, 45), labels = c("35N", "40N", "46N"), col.axis = 'white', cex.axis = 1.4, font.axis = 2); 
                   axis(1, at = c(-126, -122, -118), labels = c("126W", "122W", "118W"), col.axis = 'white', cex.axis = 1.4, font.axis = 2);
                   rect(-127, 30, -115, 50, lwd = 0, col = 'grey81'); lines(world.h.land); box(); polygon(world.h.land, col = 'grey40', border = NA); 
                   lines(world.h.borders, col = "cyan"); rect(long[1], lat[1], long[2], lat[2], lwd = 2, border = 'red');
                   text(c(-119.96, -120.90, -122.16), c(47.82, 44.67, 40.74), c('WA', 'OR', 'CA'), cex = 1.2) }, 
                   x=grconvertX(c(0.61, 0.81), from='npc'), y=grconvertY(c(0.5, 0.9), from='npc'), type='fig', pars=list( mar=c(1,1,1,1) + 0.1) )                      
if(PNG) gof()

}



# OR Central
if(PNG) png(paste0(Dir, "OR Central.png"), width = 2048, height = 2048, bg = 'grey')  else windows()
plotRAST(long = c(-126, -124), lat = c(42.5, 45))
# imap(longrange = c(-126, -124), latrange = c(42.5, 45), zoom=FALSE)
# plot.bubble.zero.cross(DatG[DatG$ptrlbs >= 0, c("SET_LONG", "SET_LAT", "ptrlbs")], add=T, scale.size = 0.007, fill.col='cyan', border.col='cyan') 
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.5))
for ( i in 1:N ) {
   DATA <- Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Area %in% AREAS[i],]
   if(BIOMASS) {
       # points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex=1.75)
	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex = 1.6)
	   points(DATA[, c("X","Y")], col = c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 1)], pch = sample(c(15,17,19),1), cex = 0.6)
    } else {
       Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14,2)]
       points(DATA[, c("X","Y")], col = Col.rnd[1], pch = sample(c(15,17,19),1), cex = 1.6)
	   points(DATA[, c("X","Y")], col = Col.rnd[2], pch = sample(c(15,17,19),1), cex = 0.6)
   }
}
if(Region == "Other" & BIOMASS)  for( i in 1:11)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.75)
if(DRAW.TRIANGULATION.MESH) {
    SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
    SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
    plot(SpatialIsoLL, draw.vertices = T, lwd = 2, size = 10, add=T)
	points(UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=2)  # Add larger vertices (knots)
}
if(PNG) gof()


# OR South, CA North
if(PNG) png(paste0(Dir, "OR South, CA North.png"), width = 2048, height = 2048, bg = 'grey')  else windows()
plotRAST(long = c(-126, -124), lat = c(40, 42.5))
plot.bubble.zero.cross(DatG[DatG$ptrlbs >= 0, c("SET_LONG", "SET_LAT", "ptrlbs")], add=T, scale.size = 0.007, fill.col='cyan', border.col='cyan')
for ( i in 1:length(AreaGroup))
     polygon(AreaGroup[[i]]$Boundary, col=col.alpha('purple', 0.5))
for ( i in 1:N ) {
   DATA <- Petrale.Results.Biomass.Stacked.Dpth.Rst[Petrale.Results.Biomass.Stacked.Dpth.Rst$Area %in% AREAS[i],]
   if(BIOMASS) {
       # points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex=1.75)
	   points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$Biomass.mt))) + 14], pch = 19, cex = 1.6)
	   points(DATA[, c("X","Y")], col = c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 1)], pch = sample(c(15,17,19),1), cex = 0.6)
    } else {
       Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14,2)]
       points(DATA[, c("X","Y")], col = Col.rnd[1], pch = sample(c(15,17,19),1), cex = 1.6)
	   points(DATA[, c("X","Y")], col = Col.rnd[2], pch = sample(c(15,17,19),1), cex = 0.6)
   }
}
if(Region == "Other" & BIOMASS)  for( i in 1:11)  points(AreaGroup[[i]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.75)
if(DRAW.TRIANGULATION.MESH) {
    SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
    SpatialIsoLL$loc <- UTM.to.LatLong(1000*SpatialIsoLL$loc)
    plot(SpatialIsoLL, draw.vertices = T, lwd = 2, size = 10, add=T)
	points(UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=2)  # Add larger vertices (knots)
}
if(PNG) gof()




   
if(F) {   

# -------- Long run time ----------   
   
# Create Yearly_Dens.png where the density is within year not over all years

# For loading the Image.RData and DateFile see the top of the file


# ******* MapDetails_List is not in Image.RData - it has to recreated here ********** !!
MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, Extrapolation_List = Extrapolation_List )


gof()
# First 1981
PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Yearly_"), 
                            Year_Set=Year_Set, Years2Include= 1981 - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(6, 6))

for( i in 1982:2015) 
   PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Year_", i, "_"), 
            Year_Set=Year_Set, Years2Include= i - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = TRUE)
gof()


 } 
   
 # ==============================================================================================
   



template <- disagg(rast(BathySmall), 10)
rr <- resample(BathySmall, template)
rr <- floor(rr/100) * 100
v <- as.polygons(rr)
plot(v, 1, col=terrain.colors(7), levels = rev(c(0, -50, -150, -400, -2600)))



 extent <- c(minLon, maxLon, minLat, maxLat)
 
BathySmall <- terra::rast(Fname)  
terra::plot(BathySmall)
     
Col.fixed = c(rgb(0/255, 57/255, 147/255), rgb(35/255, 78/255, 173/255), rgb(85/255, 109/255, 200/255), rgb(139/255, 147/255, 218/255), rgb(213/255, 217/255, 248/255))     
# df <- rasterToPoints(terra::as.contour(BathySmall))
.filled.contour(raster::rasterToPoints(as.raster(terra::as.contour(BathySmall))), levels = rev(c(0, -50, -150, -400, -2600)), col = Col.fixed)          

str(BathySmall)
str( Y <- terra::as.contour(BathySmall))

str(terra::extract(BathySmall, terra::as.points(BathySmall)))
Cont <- terra::extract(BathySmall, terra::as.contour(BathySmall))
Table(Cont$ID)



(BathySmalll.df <- as.data.frame(BathySmall, xy=TRUE))[1:4, ]
         x        y     TMP
1 -125.0996 45.89958 -1564.2
2 -125.0987 45.89958 -1564.4
3 -125.0979 45.89958 -1564.1
4 -125.0971 45.89958 -1564.3

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
# https://rspatial.org/terra/spatial/7-vectmanip.html

lib(terra)
Fname <- "TMP.tif"
BathySmall <- terra::rast(Fname)  
crs(BathySmall)
BathySmall <- project(BathySmall, '+proj=longlat')
crs(BathySmall)

BathySmall <- project(BathySmall, '+datum=WGS84')
crs(BathySmall)
# # Error: [project] cannot get output boundaries


(BathySmall.df <- as.data.frame(BathySmall, xy=TRUE))[1:4, ]
BathySmall.df$x <- BathySmall.df$x * 1
# BathySmall.df$x <- BathySmall.df$x - 0.08
# BathySmall.df$x <- BathySmall.df$x * 1.000643

BathySmall.adj <- rast(BathySmall.df)
# crs(BathySmall.adj) <- crs(BathySmall)


Col.fixed = c(rgb(0/255, 57/255, 147/255), rgb(35/255, 78/255, 173/255), rgb(85/255, 109/255, 200/255), rgb(139/255, 147/255, 218/255), rgb(213/255, 217/255, 248/255))     

BathySmall <- terra::rast(Fname)  
plot(BathySmall, col = NA)
# terra::contour(BathySmall, col = Col.fixed, add = TRUE, levels = rev(c(0, -50, -150, -400, -2600)))
terra::contour(BathySmall, col = Col.fixed, levels = rev(c(0, -50, -150, -400, -2600)), filled = TRUE, plot.title = NULL)
terra::contour(BathySmall.adj, levels = rev(c(0, -50, -150, -400, -2600)), add = TRUE)


plot(BathySmall, col = NA)
# terra::contour(BathySmall, col = Col.fixed, add = TRUE, levels = rev(c(0, -50, -150, -400, -2600)))
terra::contour(BathySmall, col = Col.fixed, levels = rev(c(0, -50, -150, -400, -2600)), filled = TRUE, plot.title = NULL)
# terra::contour(BathySmall, levels = rev(c(0, -50, -150, -400, -2600)), add = TRUE)

# v <- terra::as.contour(BathySmall, levels = rev(c(0, -50, -150, -400, -2600)))
# crs(v, proj = TRUE)
# 
# CRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84"
# w <- project(v, CRS)
# crs(w, proj = TRUE)
# 
# terra::lines(w, col ='green')
# 
# 
# BathySmall <- project(BathySmall, '+proj=longlat')


longrange <- c(-125.1, -123.8); latrange <- c(45.0, 45.9) 
minLon <- longrange[1]
maxLon <- longrange[2]
minLat <- latrange[1]
maxLat <- latrange[2]
col.imap <- rgb(244/255, 198/255, 104/255)
world.h.land.adj <- Imap::world.h.land
world.h.land.adj[,1] <- world.h.land.adj[,1] - 0.170
Imap::imap(world.h.land.adj, longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), add = TRUE, poly = col.imap, zoom = FALSE)


dev.new()
Imap::imap(list(Imap::world.h.land, Imap::world.h.borders), longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), poly = col.imap, zoom = FALSE)


# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

as.data.frame(v)
  level
1  -400
2  -150
3   -50
4     0



p <- terra::as.polygons(BathySmall, levels = rev(c(0, -50, -150, -400, -2600)))
plot(BathySmall, col = NA)
terra::contour(BathySmall, col = Col.fixed, add = TRUE, levels = rev(c(0, -50, -150, -400, -2600)))
terra::polygons(p)








 df <- rasterToPoints(raster::raster(Fname))

df <- raster::rasterToPoints(raster::raster(Fname))

Grid <- interp(df[,1], df[,2], df[,3])
 
 .filled.contour(Grid, levels = rev(c(0, -50, -150, -400, -2600)), col = Col.fixed)   
 
 
Fname <- "TMP.tif"


BathySmall <- raster::raster(Fname)  
plot(BathySmall) 
raster::filledContour(BathySmall, col =  Col.fixed, levels = rev(c(0, -50, -150, -400, -2600)), 
                plot.axes = {
                      axis(1)
                      axis(2)
                      # raster::contour(BathySmall, add = TRUE, lwd = 2)
               } )
               
               
               

BathySmall <- terra::rast(Fname)  
plot(BathySmall) 
terra::contour(BathySmall, col =  Col.fixed, levels = rev(c(0, -50, -150, -400, -2600)), filled = TRUE,
                plot.axes = {
                      axis(1)
                      axis(2)
                      # terra::contour(BathySmall, add = TRUE, lwd = 2)
               } )      

               
               
           
BathySmall <- terra::rast(Fname)  
plot(BathySmall) 
terra::contour(BathySmall, col =  Col.fixed, levels = rev(c(0, -50, -150, -400, -2600)), filled = TRUE)      


BathySmall <- raster::raster(Fname)  
plot(BathySmall, col =  Col.fixed) 
# raster::filledContour(BathySmall, col =  Col.fixed, levels = rev(c(0, -50, -150, -400, -2600)))
raster::contour(BathySmall, levels = rev(c(0, -50, -150, -400, -2600)), add = TRUE)         

              
(df <-  raster::rasterToPoints(raster::raster(Fname)))[1:3, ]
Grid <- interp(df[, 1], df[, 2], df[, 3])   



(df <-  raster::rasterToPoints(raster::raster(Fname)))[1:3, ]
BathySmall.list <- wireframe.lattice.f(df[, 1], df[, 2], df[, 3], grid.len.x = 40)
fitted.contour(BathySmall.list)

         
   
  