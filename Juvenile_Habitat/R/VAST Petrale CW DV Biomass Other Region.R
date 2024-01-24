


# ================================= WCGBTS data from the Warehouse ================================================
 
if(F) { 
    Petrale.BD.NWFSC.2017 <- WCGBTS_Combo_Bio_Data(Species = "Eopsetta jordani", YearRange = c(2003, 2017), verbose = T)
    
    Petrale.Catch.NWFSC.2017 <- WCGBTS_Combo_Catch_Wt(Species = "Eopsetta jordani", YearRange = c(2003, 2017), verbose = T)
    
    Petrale.BD.NWFSC.2017$KEY <- paste(Petrale.BD.NWFSC.2017$Year, Petrale.BD.NWFSC.2017$Vessel, Petrale.BD.NWFSC.2017$Tow)
    Petrale.Catch.NWFSC.2017$KEY <- paste(Petrale.Catch.NWFSC.2017$Year, Petrale.Catch.NWFSC.2017$Vessel, Petrale.Catch.NWFSC.2017$Tow)
    (Petrale.BD.NWFSC.2017 <- match.f( Petrale.BD.NWFSC.2017, Petrale.Catch.NWFSC.2017, "KEY", "KEY", c("Total_sp_wt_kg", "Area_Swept_ha")))[1:4,]
    
    (Petrale.BD.NWFSC.2017 <- match.f( Petrale.BD.NWFSC.2017, Petrale.Catch.NWFSC.2017, "KEY", "KEY", c("Area_Swept_ha")))[1:4,]
    
    save(Petrale.BD.NWFSC.2017, Petrale.Catch.NWFSC.2017, file = 'W:/ALL_USR/JRW/Assessment/Petrale - Melissa/R/Petrale.BD.Catch.NWFSC.2017.dmp')
    load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/R/Petrale.BD.Catch.NWFSC.2017.dmp")
}


# ============================================== VAST =====================================================================

# devtools::install_github("John-R-Wallace/R-ToolBox")
if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
if (!any(installed.packages()[, 1] %in% "RCurl")) 
         install.packages("R6")
if (!any(installed.packages()[, 1] %in% "RCurl")) 
         install.packages("curl")
if (!any(installed.packages()[, 1] %in% "RCurl")) 
         install.packages("RCurl")
if(!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
		
# install.packages('RcppEigen', repos='http://cran.rstudio.com/')
# devtools::install_github('RcppCore/RcppEigen')


require(JRWToolBox)	
lib(numDeriv)

# Using CRAN version for now - GitHub version gives an error
# lib(TMB)  # May be older on CRAN
# lib('kaskr/adcomp/TMB')
# SHA('TMB')
# 2018-03-29 13:28:09
# devtools::install_github('TMB', ref = '0085ded4048d7fbe5079616c40640dbf5982faf2')

# lib(TMBhelper)  # May be older on CRAN
# lib('kaskr/TMB_contrib_R/TMBhelper')

# geostatistical_delta-GLMM should load with VAST
#  lib('nwfsc-assess/geostatistical_delta-GLMM', 'SpatialDeltaGLMM')
SHA('SpatialDeltaGLMM') #GithubSHA1: 774b0f6ff7e019c8c6e8f2d8909ac771dcdbf0bb
# # 2018-03-29 13:24:06
# # devtools::install_github('SpatialDeltaGLMM', ref = '0085ded4048d7fbe5079616c40640dbf5982faf2')

# Jim's Utilities should load with VAST
# lib('james-thorson/Utilities', quiet = F)

# lib('james-thorson/VAST', quiet = F)
SHA('VAST')
# 2018-03-29 13:25:37
# devtools::install_github('VAST', ref = '0085ded4048d7fbe5079616c40640dbf5982faf2')

INLA:::inla.dynload.workaround()  # INLA fix on some older Linux system - does nothing on Windows


# Tantalus 
if(F) {
    
    lib(numDeriv)
    lib(TMB)
    
    # lib(kaskr/TMB_contrib_R/TMBhelper)
    devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
    
    
    # VAST
    # lib(james-thorson/Utilities)
    if (!any(installed.packages()[, 1] %in% "Utilities")) 
             devtools::install_github("james-thorson/Utilities")  
    
    # ERROR: dependencies ‘SpatialDFA’, ‘SpatialDeltaGLMM’ are not available for package ‘VAST’
    
    # lib(james-thorson/VAST)
    install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
    INLA:::inla.dynload.workaround()  # INLA fix on some older Linux system
    
    devtools::install_github("james-thorson/Spatial_DFA") 
    devtools::install_github("james-thorson/SpatialDeltaGLMM") 
    devtools::install_github("james-thorson/VAST") 
    # devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
}


Region <- c("California_current", "Other")[2]
LR  <- 2 # Length Run
LenMax <- c(16, 18, 20)[LR]
AS <- c(T,F)[1] # Area Swept
YEARS <- 2003:2015	  
DataSource <- 'WCGBTS'
IndexLoop <- 7 # 800 knots

HomeDir <- getwd()

# Load Data
if(.Platform$OS.type == "windows") {
   load("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/R/Petrale.BD.Catch.NWFSC.2017.dmp")
} else {
    options(width = 150) # Linux
    load("/more_home/h_jwallace/Petrale CW NOCV Biomass Regions n_x = 300/Petrale.BD.Catch.NWFSC.2017.dmp") 
	# options(repos=c(CRAN="http://cran.fhcrc.org", CRANextra = "http://www.stats.ox.ac.uk/pub/RWin")) # Change to CRAN repository away from the Revolution Analytics frozen mirror.
	# update.packages(ask=F)
}

# ============== Find proportion of species total weight that is less than or equal to the length specified (LenMax) =================
# Length restriction 
Petrale.BD.NWFSC.2017$WeightCalc_kg <- NA

# Males
Petrale.BD.NWFSC.2017$WeightCalc_kg[ Petrale.BD.NWFSC.2017$Sex %in% 'M'] <- 0.00000309 * Petrale.BD.NWFSC.2017$Length_cm[Petrale.BD.NWFSC.2017$Sex %in% 'M']^3.354155

# Females and large unknown sex that are assumed to be females
TF <- Petrale.BD.NWFSC.2017$Sex %in% 'F' | (Petrale.BD.NWFSC.2017$Sex %in% 'U' & Petrale.BD.NWFSC.2017$Length_cm > 50)
Petrale.BD.NWFSC.2017$WeightCalc_kg[TF] <- 0.00000208296 * Petrale.BD.NWFSC.2017$Length_cm[TF]^3.471914

# Small unknown sex - average of M & F used
TF <- Petrale.BD.NWFSC.2017$Sex %in% 'U' & Petrale.BD.NWFSC.2017$Length_cm <= 50
Petrale.BD.NWFSC.2017$WeightCalc_kg[TF] <-  mean(c(0.00000309, 0.00000208296)) * Petrale.BD.NWFSC.2017$Length_cm[TF]^mean(c(3.354155, 3.471914))

windows(); plot(Petrale.BD.NWFSC.2017$Weight_kg, Petrale.BD.NWFSC.2017$WeightCalc_kg)
Petrale.BD.NWFSC.2017[Petrale.BD.NWFSC.2017$Sex %in% 'U' & is.finite(Petrale.BD.NWFSC.2017$Length_cm), ] 
Petrale.BD.NWFSC.2017[is.na(Petrale.BD.NWFSC.2017$WeightCalc_kg),]


# Deal with bad weights - have to save and load to make this non-interactive ****** 21 entries, all in 2015, where only the sex is given *******
# badWts <- identify(Petrale.BD.NWFSC.2017$Weight_kg, Petrale.BD.NWFSC.2017$WeightCalc_kg)
# save(badWts, file = "badWts Petrale.dmp")
load("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\R\\badWts Petrale.dmp")
Petrale.BD.NWFSC.2017$Weight_kg[badWts] <- Petrale.BD.NWFSC.2017$WeightCalc_kg[badWts]

# Use calculated wts where the normal weight is missing
windows(); plot(Petrale.BD.NWFSC.2017$Weight_kg, Petrale.BD.NWFSC.2017$WeightCalc_kg)
Petrale.BD.NWFSC.2017$Weight_kg[is.na(Petrale.BD.NWFSC.2017$Weight_kg)] <- Petrale.BD.NWFSC.2017$WeightCalc_kg[is.na(Petrale.BD.NWFSC.2017$Weight_kg)]

windows(); plot(Petrale.BD.NWFSC.2017$Weight_kg, Petrale.BD.NWFSC.2017$WeightCalc_kg)
unique(Petrale.BD.NWFSC.2017[is.na(Petrale.BD.NWFSC.2017$Weight_kg), 'KEY'])

# Remove completely those tows which contain missing lengths  - 3 tows => 54 total rows 
Petrale.BD.NWFSC.2017 <- Petrale.BD.NWFSC.2017[!Petrale.BD.NWFSC.2017$KEY %in% unique(Petrale.BD.NWFSC.2017[is.na(Petrale.BD.NWFSC.2017$Weight_kg), 'KEY']), ]


Petrale.BD.NWFSC.2017 <- match.f(Petrale.BD.NWFSC.2017, aggregate(list(Total_sp_wt_sum_kg = Petrale.BD.NWFSC.2017$WeightCalc_kg), list(KEY = Petrale.BD.NWFSC.2017$KEY), sum, na.rm = TRUE), 
                           "KEY", "KEY", "Total_sp_wt_sum_kg")
plot(Petrale.BD.NWFSC.2017$Total_sp_wt_kg, Petrale.BD.NWFSC.2017$Total_sp_wt_sum_kg); abline(0,1)

# LR = Length Restricted
Petrale.BD.NWFSC.2017 <- match.f(Petrale.BD.NWFSC.2017, aggregate(list(Total_sp_wt_sum_LR_kg = Petrale.BD.NWFSC.2017$WeightCalc_kg * (Petrale.BD.NWFSC.2017$Length_cm <= LenMax )), 
           list(KEY = Petrale.BD.NWFSC.2017$KEY), sum, na.rm = TRUE), "KEY", "KEY", "Total_sp_wt_sum_LR_kg")
Petrale.BD.NWFSC.2017$Total_sp_wt_LR_kg <- Petrale.BD.NWFSC.2017$Total_sp_wt_kg * (Petrale.BD.NWFSC.2017$Total_sp_wt_sum_LR_kg/Petrale.BD.NWFSC.2017$Total_sp_wt_sum_kg)
windows(); plot(Petrale.BD.NWFSC.2017$Total_sp_wt_kg, Petrale.BD.NWFSC.2017$Total_sp_wt_LR_kg)

# DatG <- Petrale.BD.NWFSC.2017[!duplicated(Petrale.BD.NWFSC.2017$KEY), c("KEY", "Total_sp_wt_LR_kg", "Year", "Vessel", "Pass", "Latitude_dd", "Longitude_dd", "Area_Swept_ha", "Depth_ftm")]
# DatG$Depth_m <- DatG$Depth_ftm * 1.8288
# DatG$Depth_ftm <- NULL

DatG <- match.f(Petrale.Catch.NWFSC.2017, Petrale.BD.NWFSC.2017[!duplicated(Petrale.BD.NWFSC.2017$KEY), c("KEY", "Total_sp_wt_LR_kg")], "KEY", "KEY", "Total_sp_wt_LR_kg")
DatG$Total_sp_wt_LR_kg[is.na(DatG$Total_sp_wt_LR_kg)] <- 0
# Give raw data the same strata limits as the model - this is important because with Region = 'Other' the raw data defines the underlying extrapolation grid. 
# This also limits the nuumber of deep zeros, and hence the number of knots needed, since it appears that the underlying extrapolation grid is made from all the raw data, 
#           not just the data defined within the strata limits below.

# Depth limits of LR raw data
# stem(DatG$Depth_m[ DatG$Total_sp_wt_LR_kg > 0]) 
# plot(DatG$Depth_m[ DatG$Total_sp_wt_LR_kg > 0], DatG$Total_sp_wt_LR_kg[ DatG$Total_sp_wt_LR_kg > 0])

DatG <- DatG[DatG$Latitude_dd <= 48.5 & DatG$Latitude_dd >= 34.0 & DatG$Depth_m >= 55 & DatG$Depth_m <= 130, ]
windows(); plot(DatG$Total_sp_wt_kg, DatG$Total_sp_wt_LR_kg)
rm(Petrale.BD.NWFSC.2017, Petrale.Catch.NWFSC.2017)


# ================================ END - Find proportion of species total weightFind proportion of species total weight =======================================================================

# Year_Set & Years2Include
    Year_Set = seq(min(DatG$Year),max(DatG$Year))
    Years2Include = which( Year_Set %in% sort(unique(DatG$Year)))

# Coast wide only area strata in meters 
(strata.limits <- data.frame(
      'STRATA' = c("Coastwide"),
      'north_border' = c(46, 48.5, 48.5)[LR],
      'south_border' = c(35.5, 34.0, 33.5)[LR],
      'shallow_depth' = 55,
      'deep_depth' = c(130, 130, 270)[LR]
 ))

# Version <- "VAST_v3_0_0" 
# Version <- "VAST_v4_1_0" 
(Version <- substr(list.files(R.home(file.path("library", "VAST", "executables")))[length(list.files(R.home(file.path("library", "VAST", "executables"))))], 1, 11))

# for ( i in 2) {
    i <- IndexLoop

    setwd(HomeDir)  # make sure that working directory is back where it started
     
    Method = c("Grid", "Mesh")[2]
    grid_size_km = 25 # For Grid method
    n_x <- c(50, 100, 200, 300, 450, 600, 800, 900, 1200)[i] # Number of stations - for Mesh method
 
    if(.Platform$OS.type == "windows")  setMKLthreads(3) else setMKLthreads(c(5, 5, 5, 6, 7, 8, 9, 10, 12)[i])
    
	
    # Stategy Covariate with 3 levels
    # if(Method == "Mesh")
    #        DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_NS3_', DataSource, '_nx=', n_x, '/',sep='')  #  nx Mesh method
    # if(Method == "Grid")
    #    DateFile <- paste(HomeDir,'/', Sys.Date(), '_Petrale_CW_NOCV_NS3_', DataSource, '_grid_size_km=', grid_size_km, '/',sep='')    # grid size
    
    # No covariate
    # No covariate
	
	# CW = Coast Wide; NCV = No Covariate; ; DV = Depth Covariate; DataSource is data source used; Region is region used; AS = Area Swept; BM = Biomass; NV = No Vessel; NY = No Year;  VX is version X
    if(AS)  DateFile <- paste0(HomeDir,'/', Sys.Date(), '_PTRL_CW_DV_', DataSource, '_', Region, '_V4_AS_nx=', n_x, '/')  # nx Mesh method
	if(!AS) DateFile <- paste0(HomeDir,'/', Sys.Date(), '_PTRL_CW_DV_', DataSource, '_', Region, '_V4_BM_nx=', n_x, '/')  # nx Mesh method
	 
	dir.create(DateFile)
  	  
    FieldConfig = c(Omega1=1, Epsilon1=1, Omega2=1, Epsilon2=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
	RhoConfig = c(Beta1=0, Beta2=0, Epsilon1=0, Epsilon2=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
    OverdispersionConfig = c("Delta1"=1, "Delta2"=1) # Can all the vessels in logbook data be treated as random variables??
	
	# See the help for Data_Fn() and the manual on GitHub
	#  *******  ObsModel[2] = 1:  Poisson-link function that approximates a Tweedie distribution is needed for the Petrale Winter fishery *********
	#  *******  ObsModel[2] = 1:  Alternative delta-model using log-link for numbers-density and log-link for biomass per number
    ObsModel = c(2,1)  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
	# ObsModel = c(10,2) # 10 = Tweedie distribution; 2 = Link function for Tweedie distribution, necessary for ObsModel[1]=8 or ObsModel[1]=10
	# ObsModel = c(8,2) # 8 = Compound-Poisson-Gamma; 2 = Link function necessary for ObsModel[1]=8 or ObsModel[1]=10
    Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  
    # PlotResultsOnMap_Fn() says to: "Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'"
    Options <- c(SD_site_density=0, SD_site_logdensity=1, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=1, Calculate_Cov_SE=0, 
                 Calculate_Synchrony=0, Calculate_Coherence=0)

    # Save options for future records
    # Record <- ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
    # capture.output( Record, file=paste0(DateFile,"Record.txt"))
	

    Extrapolation_List <- SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn(Region=Region, strata.limits=strata.limits, observations_LL = cbind(Lat = DatG$Latitude_dd, Lon = DatG$Longitude_dd) )
	
    # if(!exists(Spatial_List)) {
       Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lat = DatG$Latitude_dd, Lon = DatG$Longitude_dd, 
             Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
             DirPath=DateFile )

       save(Spatial_List, file=paste0(DateFile, "Spatial_List.RData"))
    # }

    # Make TMB data list - ****** using tow duration for the effort instead of area sweeped ******

	# No covariate	 
	if(F) {
         
        # No duration of effort; a_i = DatG$DURATION is now: a_i = rep(1, nrow(DatG))  
	    # Area_Swept_ha for effort  ## No vessel
	    	     
	    if(AS) 
          TmbData <- VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
            "c_i"=rep(0, nrow(DatG)), "b_i"=DatG$Total_sp_wt_LR_kg, "a_i"=DatG$Area_Swept_ha, "v_i"=as.numeric(factor(DatG$Vessel))-1, 
            "s_i"=Spatial_List$knot_i-1, "t_i"=DatG$Year, "a_xl"=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
            Method=Spatial_List$Method, Options=Options ) 		
                
	    if(!AS) 
         TmbData <- VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
            c_i=rep(0, nrow(DatG)), b_i=DatG$Total_sp_wt_LR_kg, a_i=rep(1, nrow(DatG)), v_i=as.numeric(factor(DatG$Vessel))-1, 
            s_i=Spatial_List$knot_i-1, t_iz=DatG$Year, a_xl=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
            Method=Spatial_List$Method, Options=Options )
	}
	
	# Depth covariate - *** with area swept only ****
	if(T) {
	  knotsLatLong <- JRWToolBox::UTM.to.LatLong(1000 * Spatial_List$MeshList$loc_x)
	  knotsDepth <- Imap::depthMeters(knotsLatLong)
	  windows(); hist(knotsDepth)
	  knotsDepth[knotsDepth < 0] <- 0  # Set positive elevation (negative depth) of any knots on an island to zero. This can happen using Region = 'Other' - and bad things happen!!
	  # depthCovar <- SpatialDeltaGLMM::format_covariates(knotsLatLong$Lat, knotsLatLong$Long, rep(2003:2015, len = nrow(knotsLatLong)), matrix(rep(knotsDepth, times = 13), nrow=600), Extrapolation_List, Spatial_List, Year_Set = 2003:2015)
	  # depthCovar <- SpatialDeltaGLMM::format_covariates(knotsLatLong$Lat, knotsLatLong$Long, data.frame(Year = rep(2015, nrow(knotsLatLong))), data.frame(Depth_m = knotsDepth), Extrapolation_List, Spatial_List, Year_Set = 2015)
	  # depthCovar <- SpatialDeltaGLMM::format_covariates(knotsLatLong$Lat, knotsLatLong$Long, matrix(as.numeric(gl(13, nrow(knotsLatLong))), nrow=nrow(knotsLatLong)) + 2002, 
	  #             knotsDepth, Extrapolation_List, Spatial_List)
	 
	  depthCovar <- array(rep(knotsDepth, times = length(Year_Set)), dim = c(n_x, length(Year_Set), 1))
 
      TmbData <- VAST::Data_Fn(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
        c_i=rep(0, nrow(DatG)), b_i=DatG$Total_sp_wt_LR_kg, a_i=DatG$Area_Swept_ha, v_i=as.numeric(factor(DatG$Vessel)) - 1, 
        s_i=Spatial_List$knot_i - 1, t_i=DatG$Year, a_xl =Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, X_xtp = depthCovar,
        Method=Spatial_List$Method, Options=Options ) 			
	}	

    # Make TMB object
    TmbList = VAST::Build_TMB_Fn(TmbData=TmbData, RunDir=DateFile, Version=Version, RhoConfig=RhoConfig, loc_x=Spatial_List$loc_x)
    Obj = TmbList[["Obj"]]

    # Run the model
    gc()
    setwd(DateFile)
    sink("TMB_Output.txt")
    # Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList$Lower, upper=TmbList$Upper, getsd=TRUE, savedir=DateFile, bias.correct=FALSE, bias.correct.control = list(sd = TRUE, nsplit = 5))
	# Removed the parameter limits used by nlminb() since previously one parameter was hitting a bound
	Opt = TMBhelper::Optimize( obj=Obj, getsd=TRUE, savedir=DateFile, bias.correct=FALSE, bias.correct.control = list(sd = TRUE, nsplit = 5))
	# Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, 
	#                           bias.correct=TRUE, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=5, vars_to_correct="Index_cyl"))
    sink()  
    Report <- Obj$report()
	
	OptRnd <- Opt
	OptRnd <- list()
    OptRnd$par <- JRWToolBox::r(Opt$par)
    OptRnd$diagnostics <- JRWToolBox::r(Opt$diagnostics)
	OptRnd$SD <- JRWToolBox::r(summary(Opt$SD, "fixed"), 6)
   	OptRnd$Maximum_gradient_component <- Opt$max_gradient
	OptRnd$pdHess <- Opt$SD$pdHess
	OptRnd$Convergence_check <- ifelse(Opt$SD$pdHess,  { ifelse(Opt$max_gradient < 0.0001, "There is no evidence that the model is not converged", 
	                 "The model is likely not converged (the critera is a pd Hess and the max_gradient > 0.0001)") }, "The model is definitely not converged")
	OptRnd
	capture.output(OptRnd, file = file.path(DateFile, "parameter_estimates.txt"))
	
		
	# Range Raw1 and Raw2 should be inside of min and max distance of between knot locations
	r(sort(c(Range_raw1 = Report$Range_raw1, Range_raw2 = Report$Range_raw2, minDist = min(dist( Spatial_List$loc_x )), maxDist = max(dist( Spatial_List$loc_x )))))

	
    # Create MapDetails_List
	MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List ) # Make this list before the save!!!!!
      

    # Optimization result
	cat("\nnlminb() convergence (Zero indicates successful convergence):", Opt$convergence, "\n\nnlminb() message:", Opt$message, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
    sink(paste0(DateFile, "Final_Convergence_Results.txt"))
    cat("\nnlminb() convergence (Zero indicates successful convergence):", Opt$convergence, "\n\nnlminb() message:", Opt$message, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
	cat("\nRange Raw1 and Raw2 should be inside of min and max distance of between knot locations\n\n")
	print(r(sort(c(Range_raw1 = Report$Range_raw1, Range_raw2 = Report$Range_raw2, minDist = min(dist( Spatial_List$loc_x )), maxDist = max(dist( Spatial_List$loc_x ))))))
    sink()

	
	
    setwd(HomeDir)

  #######################
  # Make diagnostic plots
  #######################

   	# Plot data and knots
    try(SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, 
               Data_Geostat=data.frame( spp=rep("Petrale", nrow(DatG)), Year=DatG$Year, Catch_KG=DatG$Total_sp_wt_LR_kg, AreaSwept_km2=DatG$Area_Swept_ha/100, Vessel=DatG$Vessel, 
               Lat=DatG$Latitude_dd, Lon=DatG$Longitude_dd), PlotDir = DateFile))
   

    # Plot Anisotropy  
    SpatialDeltaGLMM::PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

    # Plot index & Table_for_SS3.csv
    SpatialDeltaGLMM::PlotIndex_Fn(DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=sort(unique(DatG$Year)), strata_names=strata.limits[,1], use_biascorr=TRUE,
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
    QQ <- SpatialDeltaGLMM::QQ_Fn(TmbData=TmbData, Report=Report, save_dir = DateFile)

	graphics.off()
	setwd(HomeDir)
	
    if(T) {   
    
      # -------- Longish run time ----------   
         
      # Create Yearly_Dens.png where the density is within year not over all years
      
      # For loading the Image.RData and DateFile see the top of the file
            
      # ******* MapDetails_List is not in Image.RData - it has to recreated here ********** !!
      # MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, Extrapolation_List = Extrapolation_List )
            
      source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\3 - Petrale Spatial Results\\Saves\\PlotMap_Fn_JRW.R")
      source("W:\\ALL_USR\\JRW\\Assessment\\Petrale - Melissa\\Org. Files Nov 2017, 2015 Data\\3 - Petrale Spatial Results\\Saves\\PlotResultsOnMap_Fn_JRW.R")
	  
      gof()
      # First 2003 with add = FALSE
      Petrale.Results.Dpth <- PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                  MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Yearly_"), 
                  Year_Set=Year_Set, Years2Include= Years2Include[1], Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                  cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = FALSE, mfrow = c(3, 6))
           
      Petrale.Results.Dpth <- cbind(Petrale.Results.Dpth, PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                  MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Year_", i, "_"), 
                  Year_Set=Year_Set, Years2Include= Years2Include[-1], Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                  cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], add = TRUE)[ ,5:(max(Years2Include) + 3)])
      gof()
      
      
   } 

   setwd(HomeDir)
 
 
  # WCGBTS VAST Grid Results with DatG using Bubble plots
  source("W:/ALL_USR/JRW/Assessment/Petrale - Melissa/Org. Files Nov 2017, 2015 Data/4 - Run VAST on WCGBTS/WCGBTS VAST Grid Results with DatG using Bubble plots.R")	


# }

# Save it all!
save.image(paste0(DateFile, "Image.RData")) 





