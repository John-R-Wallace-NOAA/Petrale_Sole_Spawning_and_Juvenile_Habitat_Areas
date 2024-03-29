PlotResultsOnMap_Fn_JRW <- function (plot_set = 3, MappingDetails, Report, Sdreport = NULL, 
    Nknots = Inf, PlotDF, MapSizeRatio = c(`Width(in)` = 4, `Height(in)` = 4), 
    Xlim, Ylim, FileName = paste0(getwd(), "/"), Year_Set = NULL, 
    Years2Include = NULL, Rescale = FALSE, Rotate = 0, Format = "png", 
    Res = 200, zone = NA, Cex = 0.01, add = FALSE, category_names = NULL, 
    textmargin = NULL, pch = NULL, Legend = list(use = FALSE, 
        x = c(10, 30), y = c(10, 30)), mfrow = NULL, plot_legend_fig = FALSE, 
    ...) 
{
    if ("D_xt" %in% names(Report)) {
        if (is.null(Year_Set)) 
            Year_Set = 1:ncol(Report$D_xt)
        if (is.null(Years2Include)) 
            Years2Include = 1:ncol(Report$D_xt)
        category_names = "singlespecies"
        Ncategories = length(category_names)
    }
    if ("D_xct" %in% names(Report)) {
        if (is.null(Year_Set)) 
            Year_Set = 1:dim(Report$D_xct)[3]
        if (is.null(Years2Include)) 
            Years2Include = 1:dim(Report$D_xct)[3]
        if (is.null(category_names)) 
            category_names = 1:dim(Report$D_xct)[2]
        Ncategories = dim(Report$D_xct)[2]
    }
    if ("D_xcy" %in% names(Report)) {
        if (is.null(Year_Set)) 
            Year_Set = 1:dim(Report$D_xcy)[3]
        if (is.null(Years2Include)) 
            Years2Include = 1:dim(Report$D_xcy)[3]
        if (is.null(category_names)) 
            category_names = 1:dim(Report$D_xcy)[2]
        Ncategories = dim(Report$D_xcy)[2]
    }
    if ("dhat_ktp" %in% names(Report)) {
        if (is.null(Year_Set)) 
            Year_Set = 1:dim(Report$dhat_ktp)[2]
        if (is.null(Years2Include)) 
            Years2Include = 1:dim(Report$dhat_ktp)[2]
        if (is.null(category_names)) 
            category_names = 1:dim(Report$dhat_ktp)[3]
        Ncategories = dim(Report$dhat_ktp)[3]
    }
    if (is.null(mfrow)) 
        mfrow = c(ceiling(sqrt(length(Years2Include))), ceiling(length(Years2Include)/ceiling(sqrt(length(Years2Include)))))
    plot_codes <- c("Pres", "Pos", "Dens", "Pos_Rescaled", "Dens_Rescaled", 
        "Eps_Pres", "Eps_Pos", "LinPred_Pres", "LinPred_Pos", 
        "Dens_CV")
    if (is.null(textmargin)) {
        textmargin <- c("Probability of encounter", "Density, ln(kg. per square km.)", 
            "Density, ln(kg. per square km.)", "", "", "", "", 
            "", "", "CV of density (dimensionless)")
    }
    if (Nknots < Inf) {
        NN_plot = RANN::kmeans(x = PlotDF[, c("Lon", "Lat")], 
            centers = Nknots, iter.max = 50, nstart = 2, trace = 0)
        Match = match(1:Nknots, NN_plot$cluster)
        PlotDF = PlotDF[Match, ]
        message("Restricted plotting locations to ", Nknots, 
            " locations")
    }
    for (cI in 1:Ncategories) {
        for (plot_num in plot_set) {
            if (plot_num == 1) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = Report$R1_xt
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = Report$R1_xct[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = Report$R1_xcy[, cI, ]
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 2) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = log(Report$R2_xt)
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = log(Report$R2_xct)[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = log(Report$R2_xcy)[, cI, ]
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 3) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = log(Report$D_xt)
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = log(Report$D_xct)[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = log(Report$D_xcy[, cI, ])
                if ("dhat_ktp" %in% names(Report)) 
                  Mat_xt = Report$dhat_ktp[, , cI]
            }
            if (plot_num == 4) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = log(Report$R2_xt + quantile(Report$R2_xt, 
                    0.25))
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = log(Report$R2_xct + quantile(Report$R2_xct, 
                    0.25))[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = log(Report$R2_xcy + quantile(Report$R2_xcy, 
                    0.25))[, cI, ]
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 5) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = log(Report$D_xt + quantile(Report$D_xt, 
                    0.25))
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = log(Report$D_xct + quantile(Report$D_xct, 
                    0.25))[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = log(Report$D_xcy + quantile(Report$D_xcy, 
                    0.25))[, cI, ]
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 6) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = Report$Epsilon1_st
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = Report$Epsilon1_sct[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  stop("Not implemented for VAST version >= 2.0.0")
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 7) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = Report$Epsilon2_st
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = Report$Epsilon2_sct[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  stop("Not implemented for VAST version >= 2.0.0")
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 8) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = Report$P1_xt
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = Report$P1_xct[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = Report$P1_xcy[, cI, ]
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 9) {
                if ("D_xt" %in% names(Report)) 
                  Mat_xt = Report$P2_xt
                if ("D_xct" %in% names(Report)) 
                  Mat_xt = Report$P2_xct[, cI, ]
                if ("D_xcy" %in% names(Report)) 
                  Mat_xt = Report$P2_xcy[, cI, ]
                if ("dhat_ktp" %in% names(Report)) 
                  stop("Not implemented for SpatialVAM")
            }
            if (plot_num == 10) {
                if (is.null(Sdreport)) 
                  stop("Must supply 'Sdreport' if 'plot_num=10'")
                if ("D_xt" %in% names(Report)) {
                  if (!("log(Index_xtl)" %in% rownames(TMB::summary.sdreport(Sdreport)))) 
                    stop("Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'SpatialDeltaGLMM'")
                  Mat_xt = array(TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport)) == 
                    "log(Index_xtl)"), ], dim = c(dim(Report$D_xt), 
                    ncol(Report$Index_tl), 2), dimnames = list(NULL, 
                    NULL, NULL, c("Estimate", "Std. Error")))[, 
                    , 1, "Std. Error"]
                }
                if ("D_xct" %in% names(Report)) {
                  if (!("log(Index_xctl)" %in% rownames(TMB::summary.sdreport(Sdreport)))) 
                    stop("Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'")
                  Mat_xt = array(TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport)) == 
                    "log(Index_xctl)"), ], dim = c(dim(Report$D_xct), 
                    dim(Report$Index_ctl)[3], 2), dimnames = list(NULL, 
                    NULL, NULL, NULL, c("Estimate", "Std. Error")))[, 
                    cI, , 1, "Std. Error"]
                }
                if ("D_xcy" %in% names(Report)) {
                  if (!("log(Index_xcyl)" %in% rownames(TMB::summary.sdreport(Sdreport)))) 
                    stop("Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'")
                  Mat_xt = array(TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport)) == 
                    "log(Index_xcyl)"), ], dim = c(dim(Report$D_xcy), 
                    dim(Report$Index_cyl)[3], 2), dimnames = list(NULL, 
                    NULL, NULL, NULL, c("Estimate", "Std. Error")))[, 
                    cI, , 1, "Std. Error"]
                }
                if ("dhat_ktp" %in% names(Report)) 
                  stop("'plot_num=10' not implemented for 'SpatialVAM'")
                Mat_xt = sqrt(exp(Mat_xt^2) - 1)
            }
            if (add == FALSE) 
                par(mfrow = mfrow)
            Return = PlotMap_Fn_JRW(MappingDetails = MappingDetails, 
                Mat = Mat_xt[, Years2Include, drop = F], PlotDF = PlotDF, 
                MapSizeRatio = MapSizeRatio, Xlim = Xlim, Ylim = Ylim, 
                FileName = paste0(FileName, plot_codes[plot_num], 
                  ifelse(Ncategories > 1, paste0("--", category_names[cI]), 
                    "")), Year_Set = Year_Set[Years2Include], 
                Rescale = Rescale, Rotate = Rotate, Format = Format, 
                Res = Res, zone = zone, Cex = Cex, textmargin = textmargin[plot_num], 
                add = add, pch = pch, Legend = Legend, mfrow = mfrow, 
                plot_legend_fig = plot_legend_fig, ...)
        }
    }
    invisible(Return)
}
