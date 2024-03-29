PlotMap_Fn_JRW <- function (MappingDetails, Mat, PlotDF, MapSizeRatio = c(`Width(in)` = 4, 
    `Height(in)` = 4), Xlim, Ylim, FileName = paste0(getwd(), 
    "/"), Year_Set, Rescale = FALSE, Rotate = 0, Format = "png", 
    Res = 200, zone = NA, Cex = 0.01, textmargin = "", add = FALSE, 
    pch = 15, outermargintext = c("Eastings", "Northings"), zlim = NULL, 
    Col = NULL, Legend = list(use = FALSE, x = c(10, 30), y = c(10, 
        30)), mfrow = c(1, 1), plot_legend_fig = FALSE, land_color = "grey", 
    ignore.na = FALSE, ...) 
{
    require(maps)
    require(mapdata)
    # on.exit(detach("package:mapdata"))
    # on.exit(detach("package:maps"), add = TRUE)
    # Mat <<- Mat
    # PlotDF <<- PlotDF
    Mat = Mat[PlotDF[, "x2i"], , drop = FALSE]
    Which = which(PlotDF[, "Include"] > 0)
    if (Rescale) 
        Mat = Mat/outer(rep(Rescale, nrow(Mat)), colMeans(Mat[Which, ]))
    f = function(Num, zlim = NULL) {
        if (is.null(zlim)) 
            Return = ((Num) - min(Num, na.rm = TRUE))/max(diff(range(Num, 
                na.rm = TRUE)), 0.01)
        if (!is.null(zlim)) 
            Return = ((Num) - zlim[1])/max(diff(zlim), 0.01)
        return(Return)
    }
    if (is.null(Col)) 
        Col = colorRampPalette(colors = c("darkblue", "blue", 
            "lightblue", "lightgreen", "yellow", "orange", "red"))
    Par = list(mfrow = mfrow, ...)
    if (!add) {
       if (Format == "png") {
           png(file = paste0(FileName, ".png"), width = Par$mfrow[2] * 
               MapSizeRatio["Width(in)"], height = Par$mfrow[1] * 
               MapSizeRatio["Height(in)"], res = Res, units = "in")
       }
       if (Format == "jpg") {
          jpeg(file = paste0(FileName, ".jpg"), width = Par$mfrow[2] * 
               MapSizeRatio["Width(in)"], height = Par$mfrow[1] * 
               MapSizeRatio["Height(in)"], res = Res, units = "in")
       }
       if (Format %in% c("tif", "tiff")) {
          tiff(file = paste0(FileName, ".tif"), width = Par$mfrow[2] * 
              MapSizeRatio["Width(in)"], height = Par$mfrow[1] * 
              MapSizeRatio["Height(in)"], res = Res, units = "in")
       }
       par(Par)
    }
    for (tI in 1:length(Year_Set)) {
        if (is.null(MappingDetails)) {
            plot(1, type = "n", ylim = Ylim, xlim = Xlim, main = "", 
                xlab = "", ylab = "")
            points(x = PlotDF[Which, "Lon"], y = PlotDF[Which, 
                "Lat"], col = Col(n = 50)[ceiling(f(Mat[Which, 
                ], zlim = zlim)[, t] * 49) + 1], cex = 0.01)
        }
        else {
            boundary_around_limits = 3
            Map = maps::map(MappingDetails[[1]], MappingDetails[[2]], 
                plot = FALSE, ylim = mean(Ylim) + boundary_around_limits * 
                  c(-0.5, 0.5) * diff(Ylim), xlim = mean(Xlim) + 
                  boundary_around_limits * c(-0.5, 0.5) * diff(Xlim), 
                fill = TRUE)
            Tmp1 <- na.omit(cbind(PID = cumsum(is.na(Map$x)), 
                POS = 1:length(Map$x), X = Map$x, Y = Map$y, 
                matrix(0, ncol = length(Year_Set), nrow = length(Map$x), 
                  dimnames = list(NULL, Year_Set))))
            TmpLL <- rbind(Tmp1, cbind(PID = max(Tmp1[, 1]) + 
                1, POS = 1:length(Which) + max(Tmp1[, 2]), X = PlotDF[Which, 
                "Lon"], Y = PlotDF[Which, "Lat"], Mat[Which, 
                ]))
            tmpUTM = TmpLL
            tmpUTM[, c("X", "Y")] = as.matrix(FishStatsUtils::Convert_LL_to_UTM_Fn(Lon = TmpLL[, 
                "X"], Lat = TmpLL[, "Y"], zone = zone, flip_around_dateline = ifelse(MappingDetails[[1]] %in% 
                c("world2", "world2Hires"), TRUE, FALSE))[, c("X", 
                "Y")])
            tmpUTM <- data.frame(tmpUTM)
            sp::coordinates(tmpUTM) = c("X", "Y")
            tmpUTM_rotated <<- maptools::elide(tmpUTM, rotate = Rotate)
            plot(1, type = "n", xlim = range(tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "x"]), ylim = range(tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "y"]), xaxt = "n", yaxt = "n")
            Col_Bin = ceiling(f(tmpUTM_rotated@data[-c(1:nrow(Tmp1)), 
                -c(1:2), drop = FALSE], zlim = zlim)[, tI] * 
                49) + 1
            if (ignore.na == FALSE && any(Col_Bin < 1 | Col_Bin > 
                50)) 
                stop("zlim doesn't span the range of the variable")
            points(x = tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "x"], y = tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "y"], col = Col(n = 50)[Col_Bin], cex = Cex, 
                pch = pch)
            lev = levels(as.factor(tmpUTM_rotated@data$PID))
            for (levI in 1:(length(lev) - 1)) {
                indx = which(tmpUTM$PID == lev[levI])
                if (var(sign(TmpLL[indx, "Y"])) == 0) {
                  polygon(x = tmpUTM_rotated@coords[indx, "x"], 
                    y = tmpUTM_rotated@coords[indx, "y"], col = land_color)
                }
                else {
                  warning("Skipping map polygons that straddle equation, because PBSmapping::convUL doesn't work for these cases")
                }
            }
        }
        title(Year_Set[tI], line = 0.1, cex.main = ifelse(is.null(Par$cex.main), 
            1.8, Par$cex.main), cex = ifelse(is.null(Par$cex.main), 
            1.8, Par$cex.main))
        box()
    }
    if (Legend$use) {
        FishStatsUtils:::smallPlot(FishStatsUtils:::Heatmap_Legend(colvec = Col(50), 
            heatrange = list(range(Mat[Which, ], na.rm = TRUE), 
                zlim)[[ifelse(is.null(zlim), 1, 2)]], dopar = FALSE), 
            x = Legend$x, y = Legend$y, mar = c(0, 0, 0, 0), 
            mgp = c(2, 0.5, 0), tck = -0.2, font = 2)
    }
    if (!add) 
        mtext(side = 1, outer = TRUE, outermargintext[1], cex = 1.75, 
            line = par()$oma[1]/2)
    if (!add) 
        mtext(side = 2, outer = TRUE, outermargintext[2], cex = 1.75, 
            line = par()$oma[2]/2)
    if (Format %in% c("png", "jpg", "tif", "tiff")) 
        # dev.off()
    if (plot_legend_fig) {
        if (Format == "png") {
            png(file = paste0(FileName, "_Legend.png", sep = ""), 
                width = 1, height = 2 * MapSizeRatio["Height(in)"], 
                res = Res, units = "in")
        }
        if (Format == "jpg") {
            jpeg(file = paste0(FileName, "_Legend.jpg", sep = ""), 
                width = 1, height = 2 * MapSizeRatio["Height(in)"], 
                res = Res, units = "in")
        }
        if (Format %in% c("tif", "tiff")) {
            tiff(file = paste0(FileName, "_Legend.tif", sep = ""), 
                width = 1, height = 2 * MapSizeRatio["Height(in)"], 
                res = Res, units = "in")
        }
        if (Format %in% c("png", "jpg", "tif", "tiff")) {
            FishStatsUtils:::Heatmap_Legend(colvec = Col(n = 50), 
                heatrange = list(range(Mat, na.rm = TRUE), zlim)[[ifelse(is.null(zlim), 
                  1, 2)]], textmargin = textmargin)
            # dev.off()
        }
    }
   invisible(data.frame(tmpUTM_rotated@coords[-(1:nrow(Tmp1)), ], TmpLL[-(1:nrow(Tmp1)), -(1:2)]))
}
