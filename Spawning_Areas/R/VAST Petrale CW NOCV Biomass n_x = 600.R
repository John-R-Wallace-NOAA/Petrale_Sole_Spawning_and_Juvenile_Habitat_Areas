

HomeDir <- getwd()

if(.Platform$OS.type == "windows") {
   setMKLthreads(5) # Windows
   load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\Petrale source_data 6 Jan 2017.dmp")
} else {
    options(width = 185) # Linux
    # setMKLthreads(8) 
    load("/more_home/h_jwallace/Petrale_500_750_1000_1250_1500_NOCV/Petrale source_data 6 Jan 2017.dmp") 
}

# Test with 10% of data:  sample_frac() is in the 'dplyr' package.
# lib(dplyr)
# DatG <- sample_frac(source_data, 0.30)

Fishery <- c('Winter', 'Summer')[1]   # Define the fishery
DatG <- source_data[source_data$SeasonCapeMendoChange %in% Fishery & !source_data$CPUE_whiting > 500,] # Two large "Hake tows" that perhaps didn't get labeled as midwater
rm(source_data)

DatG$Strategy[DatG$Strategy %in% 'HAKE'] <- "OTHER"

#  DWD   NSM OTHER 
# 1475  3684  4330 


# sum(CPUE_petrale > quantile(CPUE_petrale, 0.50) & CPUE_dover <= quantile(CPUE_dover, 0.75) & CPUE_thorny <= quantile(CPUE_thorny, 0.75) & 
#            CPUE_sablefish <= quantile(CPUE_sablefish, 0.75))/94892
# sum(CPUE_petrale <= quantile(CPUE_petrale, 0.75) & CPUE_dover > quantile(CPUE_dover, 0.50) & CPUE_thorny <= quantile(CPUE_thorny, 0.75) & 
#            CPUE_sablefish <= quantile(CPUE_sablefish, 0.75))/94892


# DatG$PC <- "PC3"
# DatG$PC[DatG$CPUE_petrale <= quantile(DatG$CPUE_petrale, 0.75) & DatG$CPUE_dover > quantile(DatG$CPUE_dover, 0.50) & 
#               DatG$CPUE_thorny <= quantile(DatG$CPUE_thorny, 0.75) & DatG$CPUE_sablefish <= quantile(DatG$CPUE_sablefish, 0.75)] <- "PC2"
# DatG$PC[DatG$CPUE_petrale > quantile(DatG$CPUE_petrale, 0.50) & DatG$CPUE_dover <= quantile(DatG$CPUE_dover, 0.75) & 
#               DatG$CPUE_thorny <= quantile(DatG$CPUE_thorny, 0.75) & DatG$CPUE_sablefish <= quantile(DatG$CPUE_sablefish, 0.75)] <- "PC1"

# devtools::install_github("John-R-Wallace/R-ToolBox")
require(JRWToolBox)
lib(numDeriv)
lib(TMB)
lib(devtools)
# devtools::install_github("james-thorson/Utilities")  
lib(ThorsonUtilities)
# devtools::install_github("james-thorson/VAST") 
lib(VAST)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
lib(TMBhelper)

# Coast wide only area strata
(strata.limits <- data.frame(
      'STRATA' = c("Coastwide"),
      'north_border' = 49.0,
      'south_border' = 32.0,
      'shallow_depth' = 10,
      'deep_depth' = 1500
    ))

Version = "VAST_v1_9_0"

for ( i in 6) {

    setwd(HomeDir)  # make sure that working directory is back where it started

    Method = c("Grid", "Mesh")[2]
    grid_size_km = 25 # For Grid method
    n_x <- c(50, 100, 200, 300, 400, 600, 800, 1000, 1200)[i] # Number of stations - for Mesh method
 
    setMKLthreads(c(5, 5, 5, 6, 7, 8, 9, 10, 12)[i])



  # Stategy Covariate with 3 levels
  # if(Method == "Mesh")
  #        DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_NS3_', Fishery, '_nx=', n_x, '/',sep='')  #  nx Mesh method
  # if(Method == "Grid")
  #    DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_NS3_', Fishery, '_grid_size_km=', grid_size_km, '/',sep='')    # grid size
  
  # No covariate
    if(Method == "Mesh")
           DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_', Fishery, '_nx=', n_x, '/',sep='')  # nx Mesh method
    if(Method == "Grid")
           DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_', Fishery, '_grid_size_km=', grid_size_km, '/',sep='')   # grid size

    dir.create(DateFile)
  
 
    FieldConfig = c(Omega1=1, Epsilon1=1, Omega2=1, Epsilon2=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
    RhoConfig = c(Beta1=0, Beta2=0, Epsilon1=0, Epsilon2=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
    OverdispersionConfig = c("Vessel"=0, "VesselYear"=1)
    #  *******  ObsModel = c(2,1) needed for Winter fishery *********
    ObsModel = c(2,1)  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
    Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  
    # PlotResultsOnMap_Fn() says to: "Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'"
    Options =  c(SD_site_density=0, SD_site_logdensity=1, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=1, Calculate_Cov_SE=0, 
                 Calculate_Synchrony=0, Calculate_Coherence=0)

    # Save options for future records
    Record = ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
    capture.output( Record, file=paste0(DateFile,"Record.txt"))

    Region <- "California_current"
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )

   # if(!exists(Spatial_List)) {
       Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=DatG$SET_LONG, Lat=DatG$SET_LAT, 
             Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
             DirPath=DateFile )

       save(Spatial_List, file=paste0(DateFile, "Spatial_List.RData"))
   # }

    # Make TMB data list - ****** using tow duration for the effort instead of area sweeped ******

  # No duration of effort; a_i = DatG$DURATION is now: a_i = rep(1, nrow(DatG))  $$$$$$$$$$$$$$ lbs to kg now fixed $$$$$$$$$$$$$$$$
  # No covariate
     TmbData = VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
        "c_i"=rep(0, nrow(DatG)), "b_i"=DatG$ptrlbs/2.20462, "a_i"=rep(1, nrow(DatG)), "v_i"=as.numeric(factor(DatG$DRVID))-1, 
        "s_i"=Spatial_List$knot_i-1, "t_i"=DatG$RYEAR, "a_xl"=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
        Method=Spatial_List$Method, Options=Options )
 
  # Strategy covariate
  #  TmbData = VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
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
    save.image(paste0(DateFile, "Image.Rdata")) 
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

    try(SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, 
               Data_Geostat=data.frame( spp=rep("Petrale", nrow(DatG)), Year=DatG[,"RYEAR"], Catch_KG=DatG[,"ptrlbs"], AreaSwept_km2=DatG[,"DURATION"], Vessel=DatG[,"DRVID"], 
               Lat=DatG[,"SET_LAT"], Lon=DatG[,"SET_LONG"], Spatial_List$loc_UTM), PlotDir=DateFile))

    # Plot Anisotropy  
    SpatialDeltaGLMM::PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

    # Plot index & Table_for_SS3.csv
    SpatialDeltaGLMM::PlotIndex_Fn(DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=sort(unique(DatG$RYEAR)), strata_names=strata.limits[,1], use_biascorr=TRUE,
                                  width = 11, height = 8)

    # Plot surface
    MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
    SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=c(3:10), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                            MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                            Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                            cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]])

    # Plot center of gravity and effective area occupied.
    # SpatialDeltaGLMM::Plot_range_shifts(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)

######################

# Load up image file, change the DateFile and run the code needed
base::load(Image.Rdata)
setwd("W:/ALL_USR/JRW/Assessment/VAST_Runs/Petrale")
DateFile = "."
SpatialDeltaGLMM::Plot_range_shifts(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)

######################

    # By year; field density, EPS, linear prediction positive catch rate and presence, posterior predictive, and over all years; posterior predictive histogram, and Q-Q plot
    # Q = SpatialDeltaGLMM::QQ_Fn( TmbData=TmbData, Report=Report, FileName_PP=paste0(DateFile,"Posterior_Predictive.jpg"), FileName_Phist=paste0(DateFile,"Posterior_Predictive-Histogram.jpg"), 
    #                           FileName_QQ=paste0(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=paste0(DateFile,"Q-Q_hist.jpg"))

}

