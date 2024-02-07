## Runs & plots linear models of multiple years/rows of lags of one vector against another
## Plots Rsquared and P values with increasing years of lag
## Simon Dedman 2018.08.30
source("/home/simon/Dropbox/Galway/Analysis/R/My Misc Scripts/lmplot.R")

lmplotlag <- function(n = 1,    # lag year(s), single or vector
                      x = NULL, # explanatory variable data
                      y = NULL, # response variable data
                      xexpvarname = "explanatory variable x", # variable name for plot header, xname in lmplot
                      yresvarname = "response variable y", # variable name for plot header, yname in lmplot
                      pngtype = "cairo-png", # plotting device, OS-sensitive
                      ... # pass to/through lmplot inc r2line pointcol pointtext pointlabs & text label params e.g. adj cex col
){
  nr2p <- data.frame(lagYr = 0, r2 = 0, p = 0) # create container for lag year, r2 & p
  nr2p <- nr2p[FALSE,] # blank it

    for(i in n){ # start loop
    expvarLagX = x[1:(length(x) - i)]
    resvarLagY = y[(i + 1):length(y)]

    # run lm, plot created & saved, r2 & p returned & appended to container at n'th row
    m <- match(i, n) # get position in n list
    nr2p[m,] <- c(i, # put n (year lag), r2 & p in mth row of nr2p dataframe
                  lmplot(x = expvarLagX,
                         y = resvarLagY,
                         xname = xexpvarname,
                         yname = paste0(yresvarname, ", ", i, " year lag"),
                         plotname = paste0(i, " year lag vs ", xexpvarname),
                         ...
                  ) # end lmplot function
    ) # end c list of year lag and lmplot returns r2 & p
  } # restart loop until end of n then close for

  # plot R2 vs lag years
  png(filename = paste0("R2_Lag_",xexpvarname, "_", yresvarname, ".png"),
      width = 1920,
      height = 1920,
      units = "px",
      pointsize = 48,
      bg = "white",
      res = NA,
      family = "",
      type = pngtype)
  par("mar" = c(4.6, 4.1, 1.8, 1))
  plot(nr2p$lagYr,
       nr2p$r2,
       type = "b",
       pch = 20,
       main = paste0("R2 values (", xexpvarname, " vs ", yresvarname, ") vs Years of Lag"),
       xlab = paste0("Years of Lag for ", yresvarname),
       ylab = "R2 value")
  dev.off() # close plotting device

  # plot P vs lag years
  png(filename = paste0("P_Lag_",xexpvarname, "_", yresvarname, ".png"),
      width = 1920,
      height = 1920,
      units = "px",
      pointsize = 48,
      bg = "white",
      res = NA,
      family = "",
      type = pngtype)
  par("mar" = c(4.6, 4.1, 1.8, 1))
  plot(nr2p$lagYr,
       nr2p$p,
       type = "b",
       pch = 20,
       main = paste0("P values (", xexpvarname, " vs ", yresvarname, ") vs Years of Lag"),
       xlab = paste0("Years of Lag for ", yresvarname),
       ylab = "P value")
  dev.off() # close plotting device

  return(nr2p) # export nr2p out of the function environment
} # close function
