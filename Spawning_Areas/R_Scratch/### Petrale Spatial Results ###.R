
###################################################################################
###         Using:                                                                                                                                                                                   ###
###                R 3.5.1                                                                                                                                                                           ###
###                FishStatsUtils ver 2.0.0      (A Jim Thorson'spackage on GitHub that is used with VAST .)                  ###
###                Imap built under R 4.3.1 (manually copied  from R 4.3.1 library folder to  R 3.5.1 library folder)    ###
###################################################################################

#  R package achive: https://cran.r-project.org/src/contrib/Archive
# install.packages("https://cran.r-project.org/src/contrib/Archive/remotes/remotes_2.1.0.tar.gz", repos = NULL)


setwd("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/R-VAST Models & New Organized Files/NEW Organized Files/3 - Petrale Spatial Results") # Change all paths as needed

library(JRWToolBox)

if (!any(installed.packages()[, 1] %in% "JRWToolBox")) {
       if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes') 
       remotes::install_github("John-R-Wallace-NOAA/JRWToolBox")
}


require(remotes)


install.packages("https://cran.r-project.org/src/contrib/Archive/maps/maps_3.3.0.tar.gz", repos = NULL) 
install_version('lattice', '0.20-41', repos = "http://cran.us.r-project.org")
install_version('mapdata', '2.3.0', repos = "http://cran.us.r-project.org")
install_version('sp', '1.4-2', repos = "http://cran.us.r-project.org")
install_version('maptools', '0.9-9', repos = "http://cran.us.r-project.org")


install_version('RandomFieldsUtils', '0.5.3', repos = "http://cran.us.r-project.org") # 1.2.5 failed in Linux

/opt/rh/devtoolset-11/root/usr/libexec/gcc/x86_64-redhat-linux/11/ld: cannot find -lgfortran
/opt/rh/devtoolset-11/root/usr/libexec/gcc/x86_64-redhat-linux/11/ld: cannot find -lquadmath


install_version('RandomFields', '3.3.14', repos = "http://cran.us.r-project.org") # 1.2.5 failed in Linux

install.packages("https://cran.r-project.org/src/contrib/Archive/mapdata/mapdata_2.3.0.tar.gz", repos = NULL) 
install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_0.9-9.tar.gz", repos = NULL) 


install.packages("https://cran.r-project.org/src/contrib/Archive/RandomFieldsUtils/RandomFieldsUtils_1.2.5.tar.gz", repos = NULL)
install.packages("https://cran.r-project.org/src/contrib/Archive/RandomFields/RandomFields_3.3.14.tar.gz", repos = NULL)
install.packages("https://github.com/James-Thorson-NOAA/FishStatsUtils/archive/refs/tags/2.0.0.tar.gz", repos = NULL) # FishStatsUtils 2.0.0
# devtools::install_github('james-thorson-NOAA/VAST', ref = 'c2c43293421f4f2f257ecd4033211ab289553357') # VAST


# lib('James-Thorson-NOAA/FishStatsUtils')  # lib() will both install and load the package. 

# VAST et al. copied from W:\MRO\MRO-3.4.2\library
# lib(SpatialDeltaGLMM)

# On Linux 
# remotes::install_github('James-Thorson-NOAA/FishStatsUtils', ref = '437341d55a23241005c171b91b4cc104238080e7') 

lib(maps) # Ver 3.3.0
lib(mapdata) # Ver 2.3.0
lib(maptools) # Ver 0.9-9
lib(RandomFieldsUtils) #1.2.5
lib(RandomFields) #3.3.14
lib(FishStatsUtils) # Ver 2.0.0

# lib(VAST)



base::load("2017-01-12_Petrale_CW_NOCV_Winter_nx=600\\Image_NO_Save_Object.Rdata")  # For this large file, void my JRWToolBox::load() which loads twice

source("Saves/ShowAreas.f.R")
source("Saves/PlotMap_Fn_JRW.R")
source("Saves/PlotResultsOnMap_Fn_JRW.R")

DateFile <- paste0(getwd(), "/2017-01-12_Petrale_CW_NOCV_Winter_nx=600/")

Year_Set = seq(min(DatG$RYEAR),max(DatG$RYEAR))
# Years2Include = which( Year_Set %in% sort(unique(DatG$RYEAR)))

str(Spatial_List$PolygonList$NN_Extrap) # Spatial_List is an output of VAST
List of 2
 $ nn.idx  : int [1:45628, 1] 294 294 294 294 294 294 294 294 294 294 ...
 $ nn.dists: num [1:45628, 1] 152 152 153 153 153 ...

cbind(Spatial_List$PolygonList$NN_Extrap$nn.idx, Spatial_List$PolygonList$NN_Extrap$nn.dists)[1:20,]
      [,1]     [,2]
 [1,]  294 152.2009
 [2,]  294 152.3827
 [3,]  294 152.5905
 [4,]  294 152.8243
 [5,]  294 153.0838
 [6,]  294 153.3690
 [7,]  294 159.9582
 [8,]  294 160.6057
 [9,]  294 161.2753
[10,]  294 161.9670
[11,]  294 162.6803



# MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
MapDetails_List = FishStatsUtils::make_map_info(Region = Region, Extrapolation_List = Extrapolation_List, NN_Extrap = Spatial_List$PolygonList$NN_Extrap) # Ignore warning


# PlotResultsOnMap_Fn() (the old version of plot_maps()) was hacked to output the data used in the figures
#  Here each figure's  limts are independent of the others  - note that the figures are in the 'DateFile' folder and the names start with "Yearly_"
#First do 1981 with add = FALSE
PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Yearly_"), 
                            Year_Set=Year_Set, Years2Include= 1981 - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(6, 6))
                            
# Then add the other years with add = TRUE
for( i in 1982:2015) 
     PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Year_", i, "_"), 
                            Year_Set=Year_Set, Years2Include= i - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = TRUE)
gof()

browseURL(paste0(DateFile,"Yearly_Dens.png"))


# Now each figure has the same limits (names start with "Field_") and the hacked results are now saved
Petrale.Results <- PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                            Year_Set=Year_Set, Years2Include= 1981:2015 - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(6, 6))
gof()

browseURL(paste0(DateFile,"Field_Dens.png"))

save(Petrale.Results, file= "Petrale.Results.RData")

# Look at the results ; 'x' /'y' are the Eastings/Northings and 'X'/'Y' are the Long/Lat. The data by year are the ???? gridded results ???? from VAST.
dim(Petrale.Results)
[1] 36051    39

Petrale.Results[1:3,]
         x        y         X        Y    X1981    X1982    X1983    X1984    X1985    X1986    X1987    X1988    X1989   X1990    X1991    X1992    X1993    X1994    X1995
1 801.2533 3353.188 -119.5098 32.01492 4.173427 4.332318 3.919155 3.923025 3.882152 4.440736 4.553762 4.233293 4.518953 4.25066 4.366067 3.870891 5.598892 3.886658 4.457932
2 835.2568 3341.309 -119.1290 32.00807 4.173427 4.332318 3.919155 3.923025 3.882152 4.440736 4.553762 4.233293 4.518953 4.25066 4.366067 3.870891 5.598892 3.886658 4.457932
3 837.1460 3340.649 -119.1079 32.00766 4.173427 4.332318 3.919155 3.923025 3.882152 4.440736 4.553762 4.233293 4.518953 4.25066 4.366067 3.870891 5.598892 3.886658 4.457932
     X1996    X1997    X1998    X1999    X2000    X2001    X2002    X2003    X2004    X2005    X2006    X2007    X2008    X2009    X2010    X2011   X2012    X2013    X2014    X2015
1 4.486117 4.460268 3.997491 3.918849 4.685592 4.753288 4.558049 4.690361 4.859815 4.738814 5.352919 5.832333 5.688602 5.536147 5.493506 5.755866 6.23616 6.881898 5.138196 6.676092
2 4.486117 4.460268 3.997491 3.918849 4.685592 4.753288 4.558049 4.690361 4.859815 4.738814 5.352919 5.832333 5.688602 5.536147 5.493506 5.755866 6.23616 6.881898 5.138196 6.676092
3 4.486117 4.460268 3.997491 3.918849 4.685592 4.753288 4.558049 4.690361 4.859815 4.738814 5.352919 5.832333 5.688602 5.536147 5.493506 5.755866 6.23616 6.881898 5.138196 6.676092


plot.WC.3.side.by.side(Petrale.Results$X, Petrale.Results$Y, Petrale.Results$X1981*1000, Contour = F, bubble = T, scale.size=0.005)

imap(longrange = c(-128.5, -116), latrange = c(32, 50), zoom=F)
plot.bubble.zero.cross(Petrale.Results[,c("X", "Y", "X1981")], scale.size = 0.01, brank.col = NULL, add=T)

plot(Petrale.Results$X, Petrale.Results$Y, add=T, pch='.')



Year.uniq <- unique(Petrale.Results$X1981) - min(unique(Petrale.Results$X1981))
r(sort(100*Year.uniq/sum(Year.uniq)),5)
hist(100*Year.uniq/sum(Year.uniq))

Year.uniq <- unique(Petrale.Results$X1981) - min(unique(Petrale.Results$X1984))
hist(Year.uniq)
r(sort(100*Year.uniq/sum(Year.uniq)),5)
hist(100*Year.uniq/sum(Year.uniq))
hist(Petrale.Results$X1984)


Year.uniq <- unique(Petrale.Results$X1982) - min(unique(Petrale.Results$X2000))
r(sort(100*Year.uniq/sum(Year.uniq)),5)
hist(100*Year.uniq/sum(Year.uniq))

Year.uniq <- unique(Petrale.Results$X1982) - min(unique(Petrale.Results$X2015))
r(sort(100*Year.uniq/sum(Year.uniq)),5)
hist(100*Year.uniq/sum(Year.uniq))


dev.new()
imap(longrange = c(-128.5, -116), latrange = c(32, 50), zoom=F)
Spawn <- Petrale.Results[Petrale.Results$X1981 > 5, c("X", "Y", "X1981")]
points(Spawn$X, Spawn$Y, col = 'red', pch='.')
text(-126, 35, "1981")

dev.new()
imap(longrange = c(-128.5, -116), latrange = c(32, 50), zoom=F)
Spawn <- Petrale.Results[Petrale.Results$X1984 > 5.5, c("X", "Y", "X1984")]
points(Spawn$X, Spawn$Y, col = 0, pch='.')
text(-126, 35, "1984")


# ------- A look at spawning grounds on the entire coast by year. ------------

load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.dmp") # Re-load() the Results if needed

dev.new()
imap(longrange = c(-128.5, -116), latrange = c(32, 50), zoom=F)

for( i in 5:39) {
   text(-126, 35, 1976 + i)
   Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), (520 - 75)/520), c(3, 4, i)]
   Spawn2 <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), (520 - 100)/520), c(3, 4, i)]
   # Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(Petrale.Results[,i], .90), c(3, 4, i)]
   points(Spawn2$X, Spawn2$Y, col = 'green4', pch='.') #  greenyellow
   points(Spawn$X, Spawn$Y, col = 'red', pch='.')
   Sys.sleep(1)
   if(i < 39) {
     points(Spawn$X, Spawn$Y, col = 0, pch='.')
     points(Spawn2$X, Spawn2$Y, col = 0, pch='.')
     text(-126, 35, 1976 + i, col=0)
   }
}

WA.OR.CA.imap <- function(..., zoom = FALSE)   imap(..., longrange = c(-128.5, -116), latrange = c(32, 50), zoom = zoom)
WA.OR.imap <- function(..., zoom = FALSE)   imap(..., longrange =c(-128, -122), latrange = c(41.5, 50), zoom = zoom)
OR.imap <- function(..., zoom = FALSE)   imap(..., longrange = c(-128, -122), latrange = c(40.5, 47.5), zoom = zoom)
CA.imap <- function(..., zoom = FALSE)   imap(..., longrange = c(-127, -116), latrange = c(32, 42.5), zoom = zoom)

OR.imap()

CA.imap()

# WA.OR.imap()

# WA.OR.CA.imap()

for( i in 5:39) {
   text(-126, 35, 1976 + i)
   Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), 0.97), c(3, 4, i)]
   # Spawn2 <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), 0.90), c(3, 4, i)]
   # points(Spawn2$X, Spawn2$Y, col = col.alpha('green4', 0.05), pch=16) #  greenyellow
   points(Spawn$X, Spawn$Y, col = col.alpha('red', 0.1), pch=16, cex= 0.5)
   Sys.sleep(1)
   if(i < 39) {
     # points(Spawn$X, Spawn$Y, col = 0, pch='.')
     # points(Spawn2$X, Spawn2$Y, col = 0, pch='.')
     text(-126, 35, 1976 + i, col=0)
   }
}





# ------ 1981 only -----------
dev.new()
imap(longrange = c(-128.5, -116), latrange = c(32, 50), zoom=F)

i <- 5
   text(-126, 35, 1976 + i)
   Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), (520 - 75)/520), c(3, 4, i, ncol(Petrale.Results))]
   Spawn2 <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), (520 - 100)/520), c(3, 4, i, ncol(Petrale.Results))]
   # Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(Petrale.Results[,i], .90), c(3, 4, i)]
   points(Spawn2$X, Spawn2$Y, col = 'green4', pch='.') 
   points(Spawn$X, Spawn$Y, col = 'red', pch='.')


imap(longrange = c(-128.5, -116), latrange = c(32, 50), zoom=F)

i <- 5
   text(-126, 35, 1976 + i)
   Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), 90, c(3, 4, i, ncol(Petrale.Results))]
   Spawn2 <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), 85, c(3, 4, i, ncol(Petrale.Results))]
   # Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(Petrale.Results[,i], .90), c(3, 4, i)]
   points(Spawn2$X, Spawn2$Y, col = 'green4', pch='.') 
   points(Spawn$X, Spawn$Y, col = 'red', pch='.')



# ------- Get the ranks by year -----------

Rank.Tab <- data.frame(Val = unique(Petrale.Results$X1981), Rank = rank(unique(Petrale.Results$X1981)))
Petrale.Rank <- renum(match.f(Petrale.Results[,c("X", "Y", "X1981")], Rank.Tab, "X1981", "Val", "Rank", round. = F))[,-3]
colnames(Petrale.Rank)[3] <- paste0("X", 1981)

for( i in 6:39) {
   bar(i, 39)
   Rank.Tab <- data.frame(Val = unique(Petrale.Results[,i]), Rank = rank(unique(Petrale.Results[,i])))
   Temp <- match.f(Petrale.Results[,c(3, 4, i)], Rank.Tab, paste0("X", i + 1976), "Val", "Rank", round. = F)
   Petrale.Rank <- cbind(Petrale.Rank, Temp[,4])
   colnames(Petrale.Rank)[ncol(Petrale.Rank)] <- paste0("X", i + 1976)
}

save(Petrale.Results, file= "Petrale.Results.dmp")
Petrale.Rank[,3:37] <- 100*Petrale.Rank[,3:37]/520
save(Petrale.Rank, file= "Petrale.Rank.dmp")

Petrale.Results$Num <- 1:nrow(Petrale.Results)


# ------- Add an area label - counting from south to north on first latitude in the area (didn't bother with centroid) ----------


load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Rank.dmp")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.dmp")

Petrale.Rank.X1981 <- sort.f(Petrale.Rank[!duplicated(Petrale.Rank$X1981), 1:3], 2)
# Petrale.Rank.X1981$X1981 <- 520* Petrale.Rank.X1981$X1981/100
Petrale.Rank.X1981$Area <- paste0("A:", 1:520)

Petrale.Rank <- match.f(Petrale.Rank, Petrale.Rank.X1981, "X1981", "X1981", "Area", round.=F)
Petrale.Results$Area <- Petrale.Rank$Area

save(Petrale.Rank, file = "W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Rank.dmp")
save(Petrale.Results, file = "W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.dmp")


# Now Spawn has an Area label

Spawn <- Petrale.Results[Petrale.Results[,i] > quantile(unique(Petrale.Results[,i]), (520 - 75)/520), c(3, 4, i, ncol(Petrale.Results))]


#   ------ Pt Conception Spawning grounds ---------


identify(Spawn$X, Spawn$Y, Spawn$Area) # Using "1981 only" figure and Spawn from above
[1] 314
Spawn[314,]   
              X        Y    X1981 Area
13515 -121.2994 34.82678 5.869214 A:93

dim(Spawn[Spawn$X1981 == Spawn$X1981[314],])
[1] 733   4

# Spawn.Pt.Con <- Spawn[Spawn$X1981 == Spawn$X1981[314],]
# points(Spawn.Pt.Con$X, Spawn.Pt.Con$Y, col = 'blue', pch='.')




# Spawn.Pt.Con <- Petrale.Results[Petrale.Results$X1981 == Spawn$X1981[314],]
Spawn.Pt.Con.Rank <- Petrale.Rank[Petrale.Results$X1981 == Spawn$X1981[314],]

Col = colorRampPalette(colors = c("darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red"))

f = function(Num, zlim = NULL) {
        if (is.null(zlim)) 
            Return = ((Num) - min(Num, na.rm = TRUE))/max(diff(range(Num, 
                na.rm = TRUE)), 0.01)
        if (!is.null(zlim)) 
            Return = ((Num) - zlim[1])/max(diff(zlim), 0.01)
        return(Return)
    }


# Show Pt Conception Spawning grounds over years

imap(longrange = c(min(Spawn.Pt.Con.Rank$X) - 0.1, max(Spawn.Pt.Con.Rank$X) + 0.4), latrange = c(min(Spawn.Pt.Con.Rank$Y) - 0.1, max(Spawn.Pt.Con.Rank$Y) + 0.1), zoom=F)

FishStatsUtils:::smallPlot(FishStatsUtils:::Heatmap_Legend(colvec = Col(n = 50), heatrange = c(0,10), dopar = FALSE), 
              x = c(70,75), y = c(30,70), mar = c(0, 0, 0, 0),  mgp = c(2, 0.5, 0), tck = -0.2, font = 2)

# subplot(FishStatsUtils:::Heatmap_Legend(colvec = Col(n = 50), heatrange = c(0,100)))

for ( i in 3:37) {
   points(Spawn.Pt.Con.Rank$X, Spawn.Pt.Con.Rank$Y, pch = 19, col = Col(n = 50)[Spawn.Pt.Con.Rank[,i]/2])
   text(-121.5, 35.18, 1978 + i,)
   Sys.sleep(0.5)
   if( i < 37) {
     for( j in 1:7)
           text(-121.5, 35.18, 1978 + i, col = 0)
   }
}

# --------------

# Petrale.Rank.Area <- Petrale.Rank[!duplicated(Petrale.Rank$Area), 3:(ncol(Petrale.Rank) - 1)]

Petrale.Rank.Area <- Petrale.Rank[!duplicated(Petrale.Rank$Area), ]
Petrale.Sorted.Mean.Rank <- sort.f(data.frame(Area = Petrale.Rank.Area$Area, Mean.Percent.Rank = apply(Petrale.Rank.Area[, 3:(ncol(Petrale.Rank) - 1)], 1, mean),
                                     SD.Percent.Rank = apply(Petrale.Rank.Area[, 3:(ncol(Petrale.Rank) - 1)], 1, sd)), 2, rev=T)
     Area Mean.Percent.Rank SD.Percent.Rank
1   A:482        98.8956044       1.4729718
2   A:487        98.4725275       1.7556921
3   A:484        98.4560440       1.7671427
4   A:486        98.1428571       2.0484807
5   A:489        97.2912088       2.6154060
6   A:379        97.0274725       2.7857659
7   A:480        96.7087912       4.7155818
8   A:485        96.6098901       3.1972668
9   A:381        95.6868132       2.8287523
10  A:375        95.5164835       4.2901964


# --------- A:482 -------------------

Spawn.A482.Rank <- Petrale.Rank[Petrale.Rank$Area == "A:482",]

Col = colorRampPalette(colors = c("darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red"))

f = function(Num, zlim = NULL) {
        if (is.null(zlim)) 
            Return = ((Num) - min(Num, na.rm = TRUE))/max(diff(range(Num, 
                na.rm = TRUE)), 0.01)
        if (!is.null(zlim)) 
            Return = ((Num) - zlim[1])/max(diff(zlim), 0.01)
        return(Return)
    }


imap(longrange = c(min(Spawn.A482.Rank$X) - 0.1, max(Spawn.A482.Rank$X) + 0.4), latrange = c(min(Spawn.A482.Rank$Y) - 0.1, max(Spawn.A482.Rank$Y) + 0.1), zoom=F)
imap(longrange = c(-128.5, -116), latrange = c(46.5, 50), zoom=F)

SpatialDeltaGLMM:::smallPlot(SpatialDeltaGLMM:::Heatmap_Legend(colvec = Col(n = 50), heatrange = c(0,10), dopar = FALSE), 
              x = c(70,75), y = c(30,70), mar = c(0, 0, 0, 0),  mgp = c(2, 0.5, 0), tck = -0.2, font = 2)

# subplot(SpatialDeltaGLMM:::Heatmap_Legend(colvec = Col(n = 50), heatrange = c(0,100)))

for ( i in 3:37) {
   points(Spawn.A482.Rank$X, Spawn.A482.Rank$Y, pch = 19, col = Col(n = 50)[Spawn.A482.Rank[,i]/2])
   text(-125.5, 47.8, 1978 + i,)
   Sys.sleep(0.5)
   if( i < 37) {
     for( j in 1:7)
           text(-125.5, 47.8, 1978 + i, col = 0)
   }
}


source('ShowAreas.f.R')

ShowAreas.f()

ShowAreas.f("A:93")
ShowAreas.f("A:93", ,c(0.1, 0.4), c(0.1, 0.4) )
ShowAreas.f("A:482")
ShowAreas.f("A:374")
ShowAreas.f("A:375")

ShowAreas.f(c("A:372", "A:373", "A:374", "A:375", "A:376", "A:377"))

ShowAreas.f(paste0("A:", 1:100), 1981:1985)

ShowAreas.f("A:93", 1981:1985)

ShowAreas.f(paste0("A:", 350:520))


# ShowAreas.f(paste0("A:", 350:520), Petrale = Petrale.Results[,-(1:2)])



# WA and OR
Imap::plotRAST(Petrale.Results[Petrale.Results[,5] > quantile(unique(Petrale.Results[,5]), 0.95), 3:4], longrange = c(-128, -122), latrange = c(41.5, 50), n.levels = 50, SoCal_1as = F)

# WA 
Imap::plotRAST(Petrale.Results[Petrale.Results[,5] > quantile(unique(Petrale.Results[,5]), 0.97), 3:4], longrange = c(-128, -122), latrange = c(46, 50), SoCal_1as = F)


# Stacked the data by year so all years can be plotted


Petrale.Results.Stacked <- data.frame(Petrale.Results[,c(3,4,5)], Year = 1981)
names(Petrale.Results.Stacked)[3] <- "CPUE" 

for ( i in 6:(ncol(Petrale.Results) - 2)) {

      Out <- data.frame(Petrale.Results[,c(3,4,i)], Year = i + 1976)
      names(Out)[3] <- "CPUE" 

      Petrale.Results.Stacked <- rbind(Petrale.Results.Stacked, Out)

}

# ----------------------

# source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Saves\\plotRAST.R") # plotRAST.R is now in my Imap package on GitHub
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Stacked.dmp")


# WA all years
plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                 longrange = c(-128, -122), latrange = c(46, 50), levels = seq(0,-3000, by=-100), SoCal_1as = F)


# ---- For 2 degree lat breaks. by = -100 for the contour breaks looks good.  For 1 degree lat breaks try by = -50 ---

# 45.9-48.5 degrees, 0.95 Quant
dev.new()
plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                   longrange = c(-126.4, -124), latrange = c(45.9, 48.5),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


# 43.9-46.1 degrees, 0.95 Quant
dev.new()
plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-127, -124), latrange = c(43.9, 46.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


# 42-44.1 degrees, 0.95 Quant
dev.new()
plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-125.7, -124), latrange = c(42, 44.1), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


dev.new()
plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-125.7, -124), latrange = c(42, 44), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)

# Example of zooming into a Raster object
plot(BathySmall, ext = extent(-125.2001, -125, 47.24987, 47.3501))






# ======================================================================= Biomass only ======================================================================================


base::load("W:\\ALL_USR\\JRW\\Assessment\\VAST_Runs\\Petrale CW NOCV Biomass n_x = 600\\Image.Rdata")
# Objects  inside of Image.Rdata
# california_current_grid
# DateFile
# DatG
# Extrapolation_List
# FieldConfig
# Fishery
# HomeDir
# Method
# n_x
# Obj
# ObsModel
# Opt
# Options
# OverdispersionConfig
# Record
# Region
# Report  # Main output from TMB for plotting beelow: Longest length in the list is: 94892
# Spatial_List
# strata.limits
# TmbData
# TmbList
# Version

source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Saves\\PlotResultsOnMap_Fn_JRW.R")
source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Saves\\PlotMap_Fn_JRW.R")
source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Saves\\plotRAST.R")

DateFile <- "W:\\ALL_USR\\JRW\\Assessment\\Petrale VAST Runs\\Petrale CW NOCV Biomass n_x = 600\\"
Year_Set <- seq(min(DatG$RYEAR),max(DatG$RYEAR))
MapDetails_List <- SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )




gof()
Petrale.Results.Biomass <- PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                            Year_Set=Year_Set, Years2Include= 1981:2015 - 1980, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(6, 6))
gof()

save(Petrale.Results.Biomass, file = "Petrale.Results.Biomass.dmp")

# ------------- Petrale biomass stacking with Area  ---------------------------------------------

# Need to rank first for Area labels then match back


load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Biomass.dmp")

# ------- Get the ranks by year for biomass -----------

Rank.Tab <- data.frame(Val = unique(Petrale.Results.Biomass$X1981), Rank = rank(unique(Petrale.Results.Biomass$X1981)))
Petrale.Rank.Biomass <- renum(match.f(Petrale.Results.Biomass[,c("X", "Y", "X1981")], Rank.Tab, "X1981", "Val", "Rank", round. = F))[,-3]
colnames(Petrale.Rank.Biomass)[3] <- paste0("X", 1981)

for( i in 6:39) {
   bar(i, 39)
   Rank.Tab <- data.frame(Val = unique(Petrale.Results.Biomass[,i]), Rank = rank(unique(Petrale.Results.Biomass[,i])))
   Temp <- match.f(Petrale.Results.Biomass[,c(3, 4, i)], Rank.Tab, paste0("X", i + 1976), "Val", "Rank", round. = F)
   Petrale.Rank.Biomass <- cbind(Petrale.Rank.Biomass, Temp[,4])
   colnames(Petrale.Rank.Biomass)[ncol(Petrale.Rank.Biomass)] <- paste0("X", i + 1976)
}


Petrale.Rank.Biomass[,3:37] <- 100*Petrale.Rank.Biomass[,3:37]/520
Petrale.Results.Biomass$Num <- 1:nrow(Petrale.Results.Biomass)

Petrale.Rank.Biomass.X1981 <- sort.f(Petrale.Rank.Biomass[!duplicated(Petrale.Rank.Biomass$X1981), 1:3], 2)
Petrale.Rank.Biomass.X1981$Area <- paste0("A:", 1:520)

Petrale.Rank.Biomass <- match.f(Petrale.Rank.Biomass, Petrale.Rank.Biomass.X1981, "X1981", "X1981", "Area", round.=F)
Petrale.Results.Biomass$Area <- Petrale.Rank.Biomass$Area

save(Petrale.Rank.Biomass, file = "W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Rank.Biomass.dmp")
save(Petrale.Results.Biomass, file = "W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Biomass.dmp")


# Finally stack Petrale biomass quants with area  - can't use biomass over all years to find the top areas 

Petrale.Results.Biomass.Stacked <- data.frame(Petrale.Results.Biomass[,c(3,4,5, ncol(Petrale.Results.Biomass))], Year = 1981)
names(Petrale.Results.Biomass.Stacked)[3] <- "Biomass" 

for ( i in 6:(ncol(Petrale.Results.Biomass) - 2)) {

      Out <- data.frame(Petrale.Results.Biomass[,c(3,4,i, ncol(Petrale.Results.Biomass))], Year = i + 1976)
      names(Out)[3] <- "Biomass" 

      Petrale.Results.Biomass.Stacked <- rbind(Petrale.Results.Biomass.Stacked, Out)
}



# Move out of log space and convert to metric tons (correcting a wrong conversion (both value & math operation) - this is now fixed in the code)
Petrale.Results.Biomass.Stacked$Biomass <- exp(Petrale.Results.Biomass.Stacked$Biomass)/2.2054/2.20462/1000 

histogram(~log(Petrale.Results.Biomass.Stacked$Biomass*1000) | factor(Petrale.Results.Biomass.Stacked$Year), type = 'count')

save(Petrale.Results.Biomass.Stacked, file = "Petrale.Results.Biomass.Stacked.dmp")



# Now find quantiles - above ranks only used for Area label 

tmp <- quantile(unique(Petrale.Results.Biomass$X1981), seq(0, 1, length=520))
Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100)

Petrale.Quant.Biomass <- renum(match.f(Petrale.Results.Biomass[,c("X", "Y", "Num", "Area", "X1981")], Quant.Tab, "X1981", "Val", "Quant", round. = T, digits = 7))[,-5]
colnames(Petrale.Quant.Biomass)[5] <- paste0("X", 1981)

for( i in 6:39) {
   bar(i, 39)
   tmp <- quantile(unique(Petrale.Results.Biomass[,i]), seq(0, 1, length=520))
   Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100)
   Temp <- match.f(Petrale.Results.Biomass[,c(3, 4, i)], Quant.Tab, paste0("X", i + 1976), "Val", "Quant", round. = T, digits = 7)
   Petrale.Quant.Biomass <- cbind(Petrale.Quant.Biomass, Temp[,4])
   colnames(Petrale.Quant.Biomass)[ncol(Petrale.Quant.Biomass)] <- paste0("X", i + 1976)
}

sum(is.na(Petrale.Quant.Biomass)) # No NA's now with using "round. = T, digits = 7"

save(Petrale.Quant.Biomass, file = "Petrale.Quant.Biomass.dmp")

# Stack the quants

Petrale.Quant.Biomass.Stacked <- data.frame(Petrale.Quant.Biomass[,c(1,2,4,5)], Year = 1981)
names(Petrale.Quant.Biomass.Stacked)[4] <- "Quants" 

for ( i in 6:(ncol(Petrale.Quant.Biomass))) {

      Out <- data.frame(Petrale.Quant.Biomass[,c(1,2,4,i)], Year = i + 1976)
      names(Out)[4] <- "Quants" 

      Petrale.Quant.Biomass.Stacked <- rbind(Petrale.Quant.Biomass.Stacked, Out)
}

Petrale.Quant.Biomass.Stacked <- renum(Petrale.Quant.Biomass.Stacked)

sum(is.na(Petrale.Quant.Biomass.Stacked)) # 0

save(Petrale.Quant.Biomass.Stacked, file="Petrale.Quant.Biomass.Stacked.dmp")

# ----------------------------------------
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Quant.Biomass.Stacked.dmp")


dev.new()
# png(paste0("Petrale, Biomass; ", "46, 48.5", ".png"), 800, 800 , bg='grey')

plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Quants > 0.95, 1:2], 
                   longrange = c(-126.4, -124), latrange = c(45.9, 48.5), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


Pet.Quant.90 <- Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Quants > 0.90, ]
Table(Pet.Quant.90$Area, Pet.Quant.90$Year)
sort(apply(Table(Pet.Quant.90$Area, Pet.Quant.90$Year), 1, function(x) sum(x>0)))


Pet.Quant.95 <- Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Quants > 0.95, ]
Table(Pet.Quant.95$Area, Pet.Quant.95$Year)
sort(apply(Table(Pet.Quant.95$Area, Pet.Quant.95$Year), 1, function(x) sum(x>0)))


# -------- ????????? Top 15% within years is looking reasonable ????????

Pet.Quant.85 <- Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Quants > 0.85, ]
Table(Pet.Quant.85$Area, Pet.Quant.85$Year)

tmp <- sort(apply(Table(Pet.Quant.85$Area, Pet.Quant.85$Year), 1, function(x) sum(x>0)))
Top.Areas.85.by.Year.at.Least.60prct.of.Years <- names(tmp[tmp >= 21])  # 21/35 = 0.60
tmp2 <- Pet.Quant.85[Pet.Quant.85$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years,]
Table(tmp2$Area, tmp2$Year)
dim(Table(tmp2$Area, tmp2$Year))


plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1], 1:2], 
                    longrange = c(-125, -124), latrange = c(45, 45.5), levels = seq(0,-3000, by=-100))

Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1]


# Top Areas 15% by Year and at Least 60% of Years

load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Quant.Biomass.Stacked.dmp")


# 45.9-48.5 degrees
dev.new()
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1], 1:2], 
                   longrange = c(-126.4, -124), latrange = c(45.9, 48.5),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


# 43.9-46.1 degrees
dev.new()
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1], 1:2], 
                  longrange = c(-127, -124), latrange = c(43.9, 46.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


# 41.9-44.1 degrees
dev.new()
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1], 1:2], 
                  longrange = c(-125.7, -124), latrange = c(41.9, 44.1), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)

# 39.9-42.1 degrees
dev.new()
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1], 1:2], 
                  longrange = c(-125.7, -124), latrange = c(39.9, 42.1), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)

# 37.9-40.1 degrees
dev.new()
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[,1], 1:2], 
                  longrange = c(-124.7, -122.5), latrange = c(37.9, 40.1), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)



# ====================================== Polygons around each area ==================================================================================================

# Try the area with the highest Biomass (A:356)

load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Biomass.Stacked.dmp")
lib(INLA)

Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Biomass > 12,] # A:356

WA.OR.CA.imap()
points(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Biomass > 12, 1:2])

OR.imap()
points(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Biomass > 12, 1:2])

plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Biomass > 12, 1:2], longrange = c(-125, -124), latrange = c(45, 45.5), levels = seq(0,-3000, by=-100))



Pet.A356 <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% 'A:356' & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]

plot(Pet.A356.ahull <-  ahull(Pet.A356$X, Pet.A356$Y, alpha = 0.35))

plot(Pet.A356[,-3])
lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix(Pet.A356[,-3]), convex=0.2, res = c(49,84))$loc, col='red')

plot(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix(Pet.A356[,-3]), convex= -0.15, res = c(49,84))$loc, col='red', type='l')
points(Pet.A356[,-3])
lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix(Pet.A356[,-3]), convex= -0.10, concave = -0.4, res = c(49,84))$loc, col='blue')
lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix(Pet.A356[,-3]), convex= -0.05, concave = -0.4, res = c(49,84))$loc, col='green')


# Area A:5 needs res = c(83,94)
for ( i in 7) {
   dev.new()
   AREA <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% paste0('A:', i) & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]
   
   plot(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.15, concave = -0.15, res = c(49,84))$loc, col='red', type='l')
   points( AREA[,-3])
   lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.05, concave = -0.4, res = c(49,84))$loc, col='blue')
   lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.025, concave = -0.4, res = c(49,84))$loc, col='green')
   lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.020, concave = -0.4, res = c(49,84))$loc, col='purple')
}


for ( i in 7:8) {
   dev.new()
   AREA <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% paste0('A:', i) & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]
   
   plot(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.15, concave = -0.15, res = 99)$loc, col='red', type='l')
   points( AREA[,-3])
   lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.05, concave = -0.4, res = 99)$loc, col='blue')
   lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.025, concave = -0.4, res = 99)$loc, col='green')
   lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.020, concave = -0.4, res = 99)$loc, col='purple')
}

for ( i in 9) {

   AREA <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% paste0('A:', i) & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]
 
   dev.new()
   plot(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.15, concave = -0.15, res = c(12,11))$loc, col='red', type='l')
   points( AREA[,-3])
   # lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.05, concave = -0.4, res = c(12,11))$loc, col='blue')
   # lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.025, concave = -0.4, res = c(12,11))$loc, col='green')
   # lines(DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.020, concave = -0.4, res = c(12,11))$loc, col='purple')
}

# --------------- Loop over each area to find the correct resolution --------------------------------------------------------------------------------------------

Top.Areas.85.by.Year.at.Least.60prct.of.Years <- data.frame(Area = Top.Areas.85.by.Year.at.Least.60prct.of.Years, Resolution = NA)


load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Biomass.Stacked.dmp")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Top.Areas.85.by.Year.at.Least.60prct.of.Years.dmp")  # saved below
lib(INLA)

########################################################################
# 14/A:435 Needs a stretch to cover all the points
# 60   - could use an oval
# 20 - manual needed
# 24 - manual needed
# 26 - two disjoint areas
# 27 - two disjoint areas
# 28 - five disjoint areas
# 30 - manual needed
# 37 - manual needed
########################################################################

gof()
# for ( j in 55:40) {
for ( j in seq(99, 9, by = -10)) {
 
  printf(j)
  #                                                        1   2   3    4    5   6   7   8   9  10  11  12  13  14  15 16   17  18  19  20   21  22  23  24  25  26
  Top.Areas.85.by.Year.at.Least.60prct.of.Years[,2] <- c( 93, 94, 93,  NA,  NA, 69, 41, NA, 41, 42, 49, 82, 63, 35, 50, 50, 42, 50,  60, NA, 43, 99, 29, NA, 88, 69, 
                     29, 69, 50, NA, 99, 59, 99, 40, 49, 69, NA, 41, 99, 50,  j, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93)
  #                  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41
  # print(Top.Areas.85.by.Year.at.Least.60prct.of.Years[15,1])

  # for ( i in nrow(Top.Areas.85.by.Year.at.Least.60prct.of.Years)) {
  try(for ( i in 41) {

     AREA <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,1] & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]
     dev.new()
     plot( AREA[,-3], xlim = c(min(AREA$X)-0.01, max(AREA$X)+0.01), ylim = c(min(AREA$Y)-0.01, max(AREA$Y)+0.01), main = paste("Resolution =", Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,2]))
     DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.020, concave = -0.40, res = Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,2])$loc
     DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])
     #  lines(DATA.inla.boundary, col='purple', type='o', pch=16)
     lines(DATA.inla.boundary, col='purple')
  })
}



# #5 with res = 64 or 84 needs adjustment of concave and convex


for ( i in 5) {

   AREA <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,1] & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]
   gof()
   dev.new()
   plot( AREA[,-3], xlim = c(min(AREA$X) - 0.01, max(AREA$X) + 0.01), ylim = c(min(AREA$Y) - 0.01, max(AREA$Y) + 0.01))
   DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.020, concave = -0.40, res = c(93, 62))$loc
   DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])
   lines(DATA.inla.boundary, col='purple')
}


 save( Top.Areas.85.by.Year.at.Least.60prct.of.Years, file="Top.Areas.85.by.Year.at.Least.60prct.of.Years.dmp")


# ----------- Save the polygons for each nursey area -----------------------------------------------------------

 # for ( i in nrow(Top.Areas.85.by.Year.at.Least.60prct.of.Years)) {

Petrale.Nursey.Area.Polygons <- list() 

 for ( i in c(1:3, 6:7, 9:12)) {

     AREA <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,1] & Petrale.Results.Biomass.Stacked$Year %in% 1981, 1:3]
     # gof()
     dev.new()
     plot( AREA[,-3], xlim = c(min(AREA$X)-0.01, max(AREA$X)+0.01), ylim = c(min(AREA$Y)-0.01, max(AREA$Y)+0.01), main = paste("Resolution =", Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,2]))
     DATA.inla.boundary <- inla.nonconvex.hull(as.matrix( AREA[,-3]), convex= -0.020, concave = -0.40, res = Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,2])$loc
     DATA.inla.boundary <- rbind(DATA.inla.boundary, DATA.inla.boundary[1,])
     lines(DATA.inla.boundary, col='purple')
     Petrale.Nursey.Area.Polygons[[Top.Areas.85.by.Year.at.Least.60prct.of.Years[i,1]]] <- DATA.inla.boundary
  }
  
save(Petrale.Nursey.Area.Polygons, file = 'Petrale.Nursey.Area.Polygons.RData'  )


browserPlot("plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[, 1], 1:2], longrange = c(-126, -124), latrange = c(44.9, 48.5), levels = seq(0,-3000, by=-200))
for ( i in 1:length(Petrale.Nursey.Area.Polygons))
                 lines(Petrale.Nursey.Area.Polygons[[i]], col = 'blue', lwd=1.5)")

browserPlot("
  WA_OR_CA_Coast()
  points(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[c(1:3, 6:7, 9:12),1], 1:2])
")

browserPlot("
  WA_OR_CA_Coast()
points(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[1:3,1], 1:2])
")

browserPlot("
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[c(1:3, 6:7, 9:12), 1], 1:2], 
                                      Petrale.Nursey.Area.Polygons[Top.Areas.85.by.Year.at.Least.60prct.of.Years[c(1:3, 6:7, 9:12), 1]], 
                                      longrange = c(-126, -124), latrange = c(44.9, 48.5), levels = seq(0,-3000, by = -200))
")

# Area: 'A:468'
browserPlot("
plotRAST(Petrale.Quant.Biomass.Stacked[Petrale.Quant.Biomass.Stacked$Area %in% Top.Areas.85.by.Year.at.Least.60prct.of.Years[11, 1], 1:2], 
                                      Petrale.Nursey.Area.Polygons[Top.Areas.85.by.Year.at.Least.60prct.of.Years[11, 1]], 
                                      longrange = c(-126, -124.8), latrange = c(47.2, 47.7), levels = seq(0,-3000, by = -100))
")



# Find the change in biomass (mt per tow) over years for each nursey area:

Petrale.Nursey.Area <- NULL
for ( i in Top.Areas.85.by.Year.at.Least.60prct.of.Years[1, 1]) {

    tmp <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked$Area %in% i, c("Area", "Year", "Biomass")]
    names(tmp)[3] <- "Biomass.mt.per.tow"
    Petrale.Nursey.Area[[i]] <- renum(tmp[!duplicated(tmp$Year),])
}




















# ================================================== OLD ===========================================================================================================================================

# ------------------- Compare CPUE with Biomass ------------------------------------

load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Stacked.dmp")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Biomass.Stacked.dmp")


LongLat <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2]
LongLat$X <- LongLat$X + 0.01


# Lat: 45.9, 48.5
# dev.new()
png(paste0("Petrale, CPUE & Biomass; ", "46, 48.5", ".png"), 640, 1024, bg='grey')

plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                   longrange = c(-126.4, -124), latrange = c(45.9, 48.5), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)
points(LongLat[LongLat$Y >= 45.9 &  LongLat$Y <= 48.5,], col = "blue", pch = 16, cex = 0.25)


for( i in seq(46, 40, by = -2))  {

  # dev.new()
  png(paste0("Petrale, CPUE & Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')

  plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-126.5, -123.5), latrange = c(i - 2.1, i + 0.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)
  points(LongLat[LongLat$Y >= i - 2.1 &  LongLat$Y <= i + 0.1,] ,col = "blue", pch = 16, cex = 0.25)

}

i <- 38
  # dev.new()
  png(paste0("Petrale, CPUE & Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')

  plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-124, -121.4), latrange = c(i - 2.1, i + 0.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)
  points(LongLat[LongLat$Y >= i - 2.1 &  LongLat$Y <= i + 0.1,] ,col = "blue", pch = 16, cex = 0.25)


i <- 36 
  # dev.new()
  png(paste0("Petrale, CPUE & Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')

  plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-123, -120), latrange = c(i - 2.1, i + 0.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)
  points(LongLat[LongLat$Y >= i - 2.1 &  LongLat$Y <= i + 0.1,] ,col = "blue", pch = 16, cex = 0.25)

if(F) {
 i <- 34
    # dev.new()
    png(paste0("Petrale, CPUE & Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')

    plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-122, -116.6), latrange = c(i - 2.3, i + 0.1),  levels = seq(0,-3000, by=-200), SoCal_1as = F, alpha = 0.75)
    points(LongLat[LongLat$Y >= i - 2.1 &  LongLat$Y <= i + 0.1,] ,col = "blue", pch = 16, cex = 0.25/4)
}

# *** PETRALE *** Alternative Southern Bight with reasonable colors - no other Petrale data is futher east or south of what is seen in the figure
i <- 34
  # dev.new()
  png(paste0("Petrale, CPUE & Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 833.33, 1000, bg='grey')

  plotRAST(Petrale.Results.Stacked[Petrale.Results.Stacked[,3] > quantile(unique(Petrale.Results.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-122, -119), latrange = c(i - 2.3, i + 0.1),  levels = seq(0,-3000, by=-200), SoCal_1as = F, alpha = 0.75)
  points(LongLat[LongLat$Y >= i - 2.1 &  LongLat$Y <= i + 0.1,] ,col = "blue", pch = 16, cex = 0.25)

gof()


# ============ Figures for Biomass only ======================

load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale.Results.Biomass.Stacked.dmp")


# Lat: 45.9, 48.5

# dev.new()
png(paste0("Petrale, Biomass; ", "46, 48.5", ".png"), 800, 800 , bg='grey')
plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2], 
                   longrange = c(-126.4, -124), latrange = c(45.9, 48.5), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)


for( i in seq(46, 40, by = -2))  {

  # dev.new()
  png(paste0("Petrale, Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')
  plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-126.5, -123.5), latrange = c(i - 2.1, i + 0.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)
}

i <- 38
  # dev.new()
  png(paste0("Petrale, Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')
  plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-124, -121.4), latrange = c(i - 2.1, i + 0.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)
 


i <- 36 
  # dev.new()
  png(paste0("Petrale, Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 800, 800, bg='grey')
  plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-123, -120), latrange = c(i - 2.1, i + 0.1),  levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)



# *** PETRALE *** Alternative Southern Bight with reasonable colors - no other Petrale data is futher east or south of what is seen in the figure
i <- 34
  # dev.new()
  png(paste0("Petrale, Biomass; ", paste(c(i - 2, i), collapse=", "), ".png"), 833.33, 1000, bg='grey')
  plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2], 
                  longrange = c(-122, -119), latrange = c(i - 2.3, i + 0.1),  levels = seq(0,-3000, by=-200), SoCal_1as = F, alpha = 0.75)

gof()

 # ======== Identify nursey areas using all years
dev.new()
# png(paste0("Petrale, Biomass; ", "46, 48.5", ".png"), 800, 800 , bg='grey')
plotRAST(Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), 1:2], 
                   longrange = c(-126.4, -124), latrange = c(45.9, 48.5), levels = seq(0,-3000, by=-100), SoCal_1as = F, alpha = 0.75)





# ------------ OLD ---------------------------------------------------------

Pet.Bio.95 <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), ]

Pet.Bio.95.North <- renum(Pet.Bio.95[Pet.Bio.95$Y >= 45.9 & Pet.Bio.95$Y <= 48.5,])

tmp <- select.pts(Pet.Bio.95.North)

#  agg the biomass by year and find proportion by year over the total sum of biomass




# -------- Contours are the 0.95 quantile areaa -------------

Pet.Biomass.95Q <- Petrale.Results.Biomass.Stacked[Petrale.Results.Biomass.Stacked[,3] > quantile(unique(Petrale.Results.Biomass.Stacked[,3]), 0.95), ]





