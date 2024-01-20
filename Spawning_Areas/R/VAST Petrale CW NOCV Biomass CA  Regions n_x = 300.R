

# devtools::install_github("John-R-Wallace/R-ToolBox")
require(JRWToolBox)
lib(numDeriv)
lib(TMB)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
lib(TMBhelper)
lib(devtools)
# devtools::install_github("james-thorson/Utilities")  
lib(ThorsonUtilities)
# devtools::install_github("james-thorson/VAST") 
lib(VAST)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
INLA:::inla.dynload.workaround()  # INLA fix on some older Linux system
lib(TMBhelper)


Region <- c("California_current", "Other")[2]

HomeDir <- getwd()

# Load LB.ShortForm.No.Hake.Strat
if(.Platform$OS.type == "windows") {
   load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\LB.ShortForm.No.Hake.Strat 22 Mar 2017.dmp")
} else {
    options(width = 150) # Linux
    load("/more_home/h_jwallace/Petrale CW NOCV Biomass n_x = 100/LB.ShortForm.No.Hake.Strat 22 Mar 2017.dmp") 
	# options(repos=c(CRAN="http://cran.fhcrc.org", CRANextra = "http://www.stats.ox.ac.uk/pub/RWin")) # Change to CRAN repository away from the Revolution Analytics frozen mirror.
	# update.packages(ask=F)
}

YEARS <- 1981:2015	  
SP <- "PTRL"
LB.ShortForm.No.Hake.Strat$PTRL.kg <- LB.ShortForm.No.Hake.Strat$ptrlbs/2.20462


#  Restricting the fleet to those vessels that catch ther top X% of the species and only using years in YEARS
cat("\nUsing species:", paste0(SP,".kg"), "\n\n")
LB.ShortForm.No.Hake.Strat <- LB.ShortForm.No.Hake.Strat[LB.ShortForm.No.Hake.Strat$RYEAR %in% YEARS, ] # Need the Years here for the Fleet
Vess.Catch <-  aggregate(list(SP.kg = LB.ShortForm.No.Hake.Strat[[paste0(SP,".kg")]]), list(DRVID = LB.ShortForm.No.Hake.Strat[['DRVID']]), sum, na.rm=T)
Vess.Catch <- sort.f(Vess.Catch, 2, rev=T)
Vess.Catch$CumSum <- cumsum(Vess.Catch$SP.kg)
Vess.Catch$PropTotal <- cumsum(Vess.Catch$SP.kg)/sum(Vess.Catch$SP.kg)
Vess.Catch[1:5,]
png("Prop Total by Vessel.png")
plot(Vess.Catch$PropTotal, xlab="Prop Catch Total by Vessel; Green lines are at 0.90, 0.95, and 0.975.")
abline(h = c(0.90, 0.95, 0.975), col='green')
graphics.off()
# dim(Vess.Catch[Vess.Catch$PropTotal <= 0.90,]) 
Fleet <- Vess.Catch[Vess.Catch$PropTotal <= 0.90, "DRVID"]
rm(Vess.Catch)


# Restricting depth, tow duration, season, and vessels that catch most of the species (see Fleet above). Also two large "Hake tows" that perhaps didn't get labeled as midwater.

Fishery <- c('Winter', 'Summer')[1]   # Define the fishery
# Depth restrictions of GT 250m and LT 500m
DatG <- LB.ShortForm.No.Hake.Strat[LB.ShortForm.No.Hake.Strat$BestDepth.m >= 250 & LB.ShortForm.No.Hake.Strat$BestDepth.m <= 500 & LB.ShortForm.No.Hake.Strat$DURATION <= 12 & 
                  LB.ShortForm.No.Hake.Strat$RYEAR %in% YEARS & LB.ShortForm.No.Hake.Strat$DRVID %in% Fleet & 
				  LB.ShortForm.No.Hake.Strat$SeasonCapeMendoChange %in% Fishery & !LB.ShortForm.No.Hake.Strat$whtlbs/LB.ShortForm.No.Hake.Strat$DURATION > 500, ]

# More restriction for poor depth reporting
DatG <- DatG[(abs(DatG$DEPTH1*1.8288 - DatG$DepthGIS.m) %<=% 250 | is.na(DatG$DEPTH1)) & DatG$DepthGIS.m %<=% 1281,]
				  
# Remove this influential point on the shelf whose reported depth clearly is too deep compared to the GIS depth estimate (192.5 meters difference)				  
DatG <- DatG[!(DatG$SET_LONG > -124.3 & DatG$SET_LAT > 45.1 & DatG$SET_LAT < 45.25),]
				  
				  
# Depth restrictions of LT 400m
# DatG <- LB.ShortForm.No.Hake.Strat[LB.ShortForm.No.Hake.Strat$DURATION <= 12 & LB.ShortForm.No.Hake.Strat$BestDepth.m <= 400 &
#                 LB.ShortForm.No.Hake.Strat$RYEAR %in% YEARS & LB.ShortForm.No.Hake.Strat$DRVID %in% Fleet & 
#				  LB.ShortForm.No.Hake.Strat$SeasonCapeMendoChange %in% Fishery & !LB.ShortForm.No.Hake.Strat$whtlbs/LB.ShortForm.No.Hake.Strat$DURATION > 500, ]

				  
rm(LB.ShortForm.No.Hake.Strat)

DatG$Strategy[DatG$Strategy %in% 'HAKE'] <- "OTHER"

Table(DatG$Strategy)
#  DWD   NSM OTHER 
# 18108 23937  2264 # Total = 44,309


# Test with 10% of data:  sample_frac() is in the 'dplyr' package.
# lib(dplyr)
# DatG <- sample_frac(Datg, 0.30)


# sum(CPUE_petrale > quantile(CPUE_petrale, 0.50) & CPUE_dover <= quantile(CPUE_dover, 0.75) & CPUE_thorny <= quantile(CPUE_thorny, 0.75) & 
#            CPUE_sablefish <= quantile(CPUE_sablefish, 0.75))/94892
# sum(CPUE_petrale <= quantile(CPUE_petrale, 0.75) & CPUE_dover > quantile(CPUE_dover, 0.50) & CPUE_thorny <= quantile(CPUE_thorny, 0.75) & 
#            CPUE_sablefish <= quantile(CPUE_sablefish, 0.75))/94892

# DatG$PC <- "PC3"
# DatG$PC[DatG$CPUE_petrale <= quantile(DatG$CPUE_petrale, 0.75) & DatG$CPUE_dover > quantile(DatG$CPUE_dover, 0.50) & 
#               DatG$CPUE_thorny <= quantile(DatG$CPUE_thorny, 0.75) & DatG$CPUE_sablefish <= quantile(DatG$CPUE_sablefish, 0.75)] <- "PC2"
# DatG$PC[DatG$CPUE_petrale > quantile(DatG$CPUE_petrale, 0.50) & DatG$CPUE_dover <= quantile(DatG$CPUE_dover, 0.75) & 
#               DatG$CPUE_thorny <= quantile(DatG$CPUE_thorny, 0.75) & DatG$CPUE_sablefish <= quantile(DatG$CPUE_sablefish, 0.75)] <- "PC1"



# Coast wide only area strata
(strata.limits <- data.frame(
      'STRATA' = c("Coastwide"),
      'north_border' = 49.0,
      'south_border' = 32.0,
      'shallow_depth' = 200,
      'deep_depth' = 550
    ))


# Version = "VAST_v2_8_0"
Version = "VAST_v3_0_0" 
# (Version <- substr(list.files(R.home(file.path("library", "VAST", "executables")))[length(list.files(R.home(file.path("library", "VAST", "executables"))))], 1, 11))

# for ( i in 2) {
    i <- 4

    setwd(HomeDir)  # make sure that working directory is back where it started
     
    Method = c("Grid", "Mesh")[2]
    grid_size_km = 25 # For Grid method
    n_x <- c(50, 100, 200, 300, 400, 600, 800, 1000, 1200)[i] # Number of stations - for Mesh method
 
    if(.Platform$OS.type == "windows")  setMKLthreads(3) else setMKLthreads(c(5, 5, 5, 6, 7, 8, 9, 10, 12)[i])
    
	
    # Stategy Covariate with 3 levels
    # if(Method == "Mesh")
    #        DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_NS3_', Fishery, '_nx=', n_x, '/',sep='')  #  nx Mesh method
    # if(Method == "Grid")
    #    DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_NS3_', Fishery, '_grid_size_km=', grid_size_km, '/',sep='')    # grid size
    
    # No covariate
    # No covariate
	
	if(Region == "California_current") {
        if(Method == "Mesh")
             DateFile <- paste0(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_', Fishery, '_CA_Curr_nx=', n_x, '/')  # nx Mesh method
        if(Method == "Grid")
             DateFile <- paste0(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_', Fishery, '_CA_Curr_km=', grid_size_km, '/')   # grid size
    }
	
	if(Region == "Other") {
        if(Method == "Mesh")
             DateFile <- paste0(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_', Fishery, '_Other_nx=', n_x, '/')  # nx Mesh method
        if(Method == "Grid")
             DateFile <- paste0(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_', Fishery, '_Other_km=', grid_size_km, '/')   # grid size
    }
	
	dir.create(DateFile)
  	
  
    FieldConfig = c(Omega1=1, Epsilon1=1, Omega2=1, Epsilon2=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
    RhoConfig = c(Beta1=0, Beta2=0, Epsilon1=0, Epsilon2=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
    OverdispersionConfig = c("Delta1"=1, "Delta2"=1) # Can all the vessels in logbook data be treated as random variables??
    #  *******  ObsModel[2] = 1:  Poisson-link function that approximates a Tweedie distribution is needed for the Petrale Winter fishery *********
    ObsModel = c(2,1)  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
    Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  
    # PlotResultsOnMap_Fn() says to: "Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'"
    Options <- c(SD_site_density=0, SD_site_logdensity=1, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=1, Calculate_Cov_SE=0, 
                 Calculate_Synchrony=0, Calculate_Coherence=0)

    # Save options for future records
    Record <- ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
    capture.output( Record, file=paste0(DateFile,"Record.txt"))

	
    if(Region == "California_current") 
         Extrapolation_List <- SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )	
	
	if(Region == "Other") 
      Extrapolation_List <- SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn(Region=Region, strata.limits=strata.limits, observations_LL = cbind(Lat = DatG$SET_LAT, Lon = DatG$SET_LONG) )
	
   # if(!exists(Spatial_List)) {
       Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=DatG$SET_LONG, Lat=DatG$SET_LAT, 
             Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
             DirPath=DateFile )

       save(Spatial_List, file=paste0(DateFile, "Spatial_List.RData"))
   # }

    # Make TMB data list - ****** using tow duration for the effort instead of area sweeped ******

  # No duration of effort; a_i = DatG$DURATION is now: a_i = rep(1, nrow(DatG))  $$$$$$$$$$$$$$ lbs to kg now fixed $$$$$$$$$$$$$$$$
  # No covariate
     TmbData <- VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
        "c_i"=rep(0, nrow(DatG)), "b_i"=DatG$PTRL.kg, "a_i"=rep(1, nrow(DatG)), "v_i"=as.numeric(factor(DatG$DRVID))-1, 
        "s_i"=Spatial_List$knot_i-1, "t_i"=DatG$RYEAR, "a_xl"=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
        Method=Spatial_List$Method, Options=Options )
 
  # Strategy covariate
  #  TmbData <- VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
  #      "c_i"=as.numeric(factor(DatG$Strategy)) - 1, "b_i"=DatG$ptrlbs/2.20462, "a_i"=rep(0, nrow(DatG)), "v_i"=as.numeric(factor(DatG$DRVID))-1, 
  #      "s_i"=Spatial_List$knot_i-1, "t_i"=DatG$RYEAR, "a_xl"=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
  #      Method=Spatial_List$Method, Options=Options )

    # Make TMB object
    TmbList = VAST::Build_TMB_Fn(TmbData=TmbData, RunDir=DateFile, Version=Version, RhoConfig=RhoConfig, loc_x=Spatial_List$loc_x)
    Obj = TmbList[["Obj"]]

    # Run model
    gc()
    setwd(DateFile)
    sink("TMB_Output.txt")
    Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=FALSE, bias.correct.control = list(sd = TRUE, nsplit = 5), loopnum = 3)
    sink()  
    Report = Obj$report()

    # Save stuff
	MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List ) # Make this list before the save!!!!!
    save.image(paste0(DateFile, "Image.RData")) 
    # Save = list(Opt=Opt, Report=Report, ParHat=Obj$env$parList(Opt$par), TmbData=TmbData)
    # save(Save, file=paste0(DateFile, "Save.RData"))
    

    # Optimize result
    sink(paste0(DateFile, "Final_Convergence_Results.txt"))
    cat("\nnlminb() convergence (Zero indicates successful convergence):", Opt$convergence, "\n\nnlminb() message:", Opt$message, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
    sink()

    
    setwd(HomeDir)

  ################
  # Make diagnostic plots
  ################

    # Plot settings
    Year_Set = seq(min(DatG$RYEAR),max(DatG$RYEAR))
    Years2Include = which( Year_Set %in% sort(unique(DatG$RYEAR)))

	# Plot data and knots
    try(SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, 
               Data_Geostat=data.frame( spp=rep("Petrale", nrow(DatG)), Year=DatG[,"RYEAR"], Catch_KG=DatG[,"ptrlbs"], AreaSwept_km2=DatG[,"DURATION"], Vessel=DatG[,"DRVID"], 
               Lat=DatG[,"SET_LAT"], Lon=DatG[,"SET_LONG"], Spatial_List$loc_UTM), PlotDir=DateFile))

    # Plot Anisotropy  
    SpatialDeltaGLMM::PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

    # Plot index & Table_for_SS3.csv
    SpatialDeltaGLMM::PlotIndex_Fn(DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=sort(unique(DatG$RYEAR)), strata_names=strata.limits[,1], use_biascorr=TRUE,
                                  width = 11, height = 8)

    # Plot surface
    SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=c(1:5,8:10), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                            Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]])

    # Plot center of gravity and effective area occupied.
    SpatialDeltaGLMM::Plot_range_shifts(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)

    ######################
    
    # Load up image file, change the DateFile and run the code needed
    # base::load(Image.RData)
    # setwd("W:/ALL_USR/JRW/Assessment/VAST_Runs/Petrale")
    # DateFile = "."
    # SpatialDeltaGLMM::Plot_range_shifts(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)
    
    ######################
    
    # By year; field density, EPS, linear prediction positive catch rate and presence, posterior predictive, and over all years; posterior predictive histogram, and Q-Q plot
    Q = SpatialDeltaGLMM::QQ_Fn( TmbData=TmbData, Report=Report, FileName_PP=paste0(DateFile,"Posterior_Predictive.jpg"), FileName_Phist=paste0(DateFile,"Posterior_Predictive-Histogram.jpg"), 
                               FileName_QQ=paste0(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=paste0(DateFile,"Q-Q_hist.jpg"))

	graphics.off()
# }
