# 2024-02-06 barplots maps####
# Need to convert to ggplot
# xyz file isn't complicated? make.xyz
# draw.barplot2D, see file in /home/simon/Dropbox/Galway/Analysis/R/My Misc Scripts/
# feeds off barplot2D, same location

barplot2dMap <- function(x, # dataframe
                         latcol = "lat",
                         loncol = "lon",
                         origin = c(0,0),
                         cellsize = c(1,1), # latlon degrees
                         groupcol = "kmeans2cluster", # col name in x
                         mycolours = c("#D55E00", "#0072B2"),
                         legendtitle = "2D Barplot",
                         zCol = "Count",
                         zFun = "sum",
                         baseplot = NULL, # From gbm.basemap output. sf::st_read(dsn = file.path("CroppedMap", "Crop_Map.shp"), layer = "Crop_Map", quiet = TRUE)
                         xlim = c(min(x[, loncol], na.rm = TRUE),
                                  max(x[, loncol], na.rm = TRUE)),
                         ylim = c(min(x[, latcol], na.rm = TRUE),
                                  max(x[, latcol], na.rm = TRUE)),
                         lon1 = min(x[, loncol], na.rm = TRUE),
                         lon2 = max(x[, loncol], na.rm = TRUE),
                         lat1 = min(x[, latcol], na.rm = TRUE),
                         lat2 = max(x[, latcol], na.rm = TRUE),
                         bathyres = 1, # resolution for getNOAA.bathy
                         bathysavepath = NULL, # directory for NOAA basemap data, will be created if doesn't exist
                         legendloc = "topleft",
                         pngwidth = 7, # width in pngunits
                         pngheight = 7, # height in pngunits
                         pngunits = "in", # units param from png, options are "in", "mm"
                         saveloc = NULL, # directory for saved map, will be created if doesn't exist
                         plotname = paste0(lubridate::today(), "_2DBarplot_Count")) {
  library(dplyr)
  library(tidyverse)
  library(marmap)
  library(lubridate)
  if (!file.exists(bathysavepath)) dir.create(bathysavepath)
  if (!file.exists(saveloc)) dir.create(saveloc)
  # get bathymetry data
  b = marmap::getNOAA.bathy(lon1 = lon1,
                            lon2 = lon2,
                            lat1 = lat1,
                            lat2 = lat2,
                            resolution = bathyres,
                            keep = TRUE,
                            path = bathysavepath)

  xy = cbind(x[,loncol], x[,latcol])
  adminCells <-  t(apply(xy, 1, function(z) cellsize/2 + origin + cellsize*(floor((z - origin)/cellsize)))) # data to cell bins
  x$X <- adminCells[, 1]
  x$Y <- adminCells[, 2]
  x$Cell <- paste(x$X, x$Y)

  cellSummaries <- x %>%
    group_by(Cell, !!sym(groupcol)) %>% # group by cell & groupcol
    summarise(Count = n(), # add Count
              !!zCol := match.fun(zFun)(!!sym(zCol), na.rm = T), # output colname zCol created as zFun on zCol
              # https://stackoverflow.com/questions/62906259/r-user-defined-dynamic-summary-function-within-dplyrsummarise#62906361
              across(c(X, Y), first)) %>% # add back X & Y for xyz fun
    ungroup()

  xyz <- make.xyz(x = cellSummaries$X,
                  y = cellSummaries$Y,
                  z = cellSummaries %>% pull(zCol),
                  group = cellSummaries %>% pull(groupcol),
                  FUN = noquote(zFun)) # should do nothing, cellSummaries should be 1:1 size for xyz so no summarising in xyz needed

  png(filename = paste0(saveloc, plotname, ".png"),
      width = pngwidth, #NA default. Manually adjust plot box in RStudio after ggplot()
      height = pngheight, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
      units = pngunits, # 2023-05-05 now in mm, was px
      res = 300, #ppi (=dpi?); 2023-05-05 addition. Makes lines really fat and filesize small, oddly
      bg = "white",
      family = "",
      type = "cairo-png") # 2023-05-12 was cairo-png but fails for tiff
  plot(NA, # make extents, graphics::plot.default
       xlim = xlim,
       ylim = ylim,
       xlab = "longitude",
       ylab = "latitude",
       cex.axis = 2, # put this in 3 places and it still doesn't affect things. I think this works here but is then overwritten.
       cex.lab = 2,
       bg = 'white') # does nothing?
  marmap::plot.bathy(b, deep = -200, shallow = -200, step = 0, col = "black") # add 200m bathymetry contour
  mapplots::draw.barplot2D(xyz$x, xyz$y, xyz$z,
                           width = cellsize[1], height = cellsize[2], scale = T, # scale = sum of Z values. Fine for count, CPUE, fish age?, but not good for depth, sst?
                           cex.axis = 2,
                           cex.lab = 2,
                           col = mycolours, col.frame = NA, lwd.frame = 1, silent = T, border = NA) # ... border density lwd
  plot(baseplot, # add coastline, overlaps & masks data on land
       type = "poly",
       xlim = xlim,
       ylim = ylim,
       col = 'grey',
       bg = 'white', # does nothing when added?
       cex.axis = 2,
       cex.lab = 2,
       add = TRUE)
  legend(x = legendloc,
         legend = colnames(xyz$z),
         fill = mycolours,
         bg = "white",
         inset = 0.02,
         title = legendtitle,
         title.adj = 0)
  dev.off()
}
