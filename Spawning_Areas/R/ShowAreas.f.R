
ShowAreas.f <- function(Area = paste0("A:", 1:520), Years = 1981:2015, LongSpread = c(1, 1), LatSpread = c(1, 1), Petrale = Petrale.Rank) {

   Col = colorRampPalette(colors = c("darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red"))

   f = function(Num, zlim = NULL) {
        if (is.null(zlim)) 
            Return = ((Num) - min(Num, na.rm = TRUE))/max(diff(range(Num, 
                na.rm = TRUE)), 0.01)
        if (!is.null(zlim)) 
            Return = ((Num) - zlim[1])/max(diff(zlim), 0.01)
        return(Return)
    }


   Spawn <- Petrale[Petrale$Area %in% Area,]
   imap(longrange = c(min(Spawn$X) - LongSpread[1], max(Spawn$X) + LongSpread[2]), latrange = c(min(Spawn$Y) - LatSpread[1], max(Spawn$Y) + LatSpread[2]), zoom=F)
  
   
   SpatialDeltaGLMM:::smallPlot(SpatialDeltaGLMM:::Heatmap_Legend(colvec = Col(n = 50), heatrange = c(0,10), dopar = FALSE), 
              x = c(70,75), y = c(30,70), mar = c(0, 0, 0, 0),  mgp = c(2, 0.5, 0), tck = -0.2, font = 2)

   for ( i in Years ) {
      points(Spawn$X, Spawn$Y, pch = 19, col = Col(n = 50)[Spawn[, i - 1978]/2])
      text(min(Spawn$X) - LongSpread[2]/6, min(Spawn$Y) - LatSpread[1]/5, i,)
      Sys.sleep(0.5)
      if( i < max(Years)) {
        for( j in 1:7)
              text(min(Spawn$X) - LongSpread[2]/6, min(Spawn$Y) - LatSpread[1]/5, i, col = 0)
        }
    }
}
