# Hans' mapplots draw.bubble code used on my DATRAS CPUE data
# Simon Dedman, 27/2/2017

# Hans exmaple:
data(landings) #lat long species liveweight
data(coast)
xlim <- c(-12, -5)
ylim <- c(50, 56)
agg <- aggregate(list(z = landings$LiveWeight),
                 list(x = landings$Lon, y = landings$Lat), sum)
# aggregate is kinda like pivottable, arranges data (1st term)
# into groups (2nd term) & optionally applies function (3rd term)
basemap(xlim, ylim, main = "Gadoid landings")
draw.shape(coast, col = "cornsilk")
draw.bubble(agg$x, agg$y, agg$z, maxradius = 0.5, pch = 21, bg = "#00FF0050")
legend.z <- round(max(agg$z)/1000,0)
legend.z <- round(max(agg$z),0)
legend.bubble("topright", z = legend.z, maxradius = 0.5, inset = 0.02,
              bg = "lightblue", txt.cex = 0.8, pch = 21, pt.bg = "#00FF0050")

# My data, all years
CPUE <- read.csv("/home/simon/Documents/Si Work/Galway PhD/Data/ICES Datras/I.S. AllData MyRays LatLon & No Only.csv")
xlim2 <- range(CPUE[,2])
ylim2 <- range(CPUE[,1])
agg2 <- aggregate(list(z = CPUE$TotalNo),
                  list(x = CPUE$Midpoint.Lon, y = CPUE$Midpoint.Lat), sum)
par(mar = c(2.3, 2.2, 1.5, 0), # margin space buffer around plot
    oma = c(1.5, 1.5, 0.5, 0.5),
    mgp = c(1.4, 0.5, 0)) # dist of lat/long labels,markers,line from axis
basemap(xlim2, ylim2, main = "ICES IBTS Ray catches, 1993-2014")
draw.shape(coast, col = "cornsilk")
draw.bubble(agg2$x, agg2$y, agg2$z, maxradius = 0.5, pch = 21, bg = "#00FF0050")
legend.z2 <- round(max(agg2$z, na.rm = TRUE)/1,0) # tweak divisor for size
legend.bubble("bottomright", z = legend.z2, maxradius = 0.5, inset = 0.02,
              bg = "lightblue", txt.cex = 0.8, pch = 21, pt.bg = "#00FF0050", n = 8)

# My data, all years
# Need to split into years somehow. agg & draw.bubble as loops through years.
CPUE2 <- read.csv("/home/simon/Dropbox/Galway/Project Sections/2. Spatial subsets inc fishery data/Data/DATRAS/IS 4sp Yr TotalNo.csv")
xlim3 <- range(CPUE2[,2])
ylim3 <- range(CPUE2[,1])
years <- sort(unique(CPUE2$Year))
agg3 <- aggregate(list(z = CPUE2$TotalNo),
                  list(x = CPUE2$Midpoint.Lon, y = CPUE2$Midpoint.Lat), sum)

length(years) #22
library("mapplots")
data(coast)
# get max yearly count value
CPUEyears <- aggregate(list(z = CPUE2$TotalNo),
                       list(x = CPUE2$Year), sum)
CPUEmax <- max(CPUEyears$z)

par(mfrow = c(1,1)) # set matrix
par(mar = c(2.3, 2.2, 1.5, 0), # margin space buffer around plot
    oma = c(1.5, 1.5, 0.5, 0.5),
    mgp = c(1.4, 0.5, 0)) # dist of lat/long labels,markers,line from axis
for (i in years) {
  forCPUE <- CPUE2[CPUE2$Year == i,]
  agg3 <- aggregate(list(z = forCPUE$TotalNo),
                    list(x = forCPUE$Midpoint.Lon, y = forCPUE$Midpoint.Lat), sum)
  basemap(xlim3, ylim3, main = paste("ICES IBTS Ray catches, ", i, sep = ""))
  draw.shape(coast, col = "cornsilk")
  # zeroes and CPUEmax added to put the max value offscreen to bind max values for scaling
  draw.bubble(c(agg3$x,0), c(agg3$y,0), c(agg3$z, CPUEmax), maxradius = 0.5, pch = 21, bg = "#00FF0050")
  legend.z2 <- round(max(agg3$z, na.rm = TRUE)/1,0) # tweak divisor for size
  legend.bubble("bottomright", z = legend.z2, maxradius = 0.5, inset = 0.02,
                bg = "lightblue", txt.cex = 0.8, pch = 21, pt.bg = "#00FF0050", n = 8)
}
# axes plotted each time.
# ditto legend.
# values scaled to year max to all years max.
# CPUEmax doesn't work correctly, too big

# max objects with aggregate XY values for each year
for (i in years) {
  forCPUE <- CPUE2[CPUE2$Year == i,]
  assign(paste("agg_", i, sep = ""),
         aggregate(list(z = forCPUE$TotalNo),
                   list(x = forCPUE$Midpoint.Lon, y = forCPUE$Midpoint.Lat), sum)
  )
}
# make blank df for each year
yearmax <- data.frame(years, maxval = rep(NA, length(years)))
# get max of each yearly object's aggregated CPUE
for (i in 1:length(years)) {
  yearmax[i,2] <- max(get(paste("agg_", years[i], sep = ""))$z)
}
max(yearmax[,2]) # 2131

for (i in years) {
  forCPUE <- CPUE2[CPUE2$Year == i,]
  agg3 <- aggregate(list(z = forCPUE$TotalNo),
                    list(x = forCPUE$Midpoint.Lon, y = forCPUE$Midpoint.Lat), sum)
  basemap(xlim3, ylim3, main = paste("ICES IBTS Ray catches, ", i, sep = ""))
  draw.shape(coast, col = "cornsilk")
  # zeroes and maxyearmax added to put the max value offscreen to bind max values for scaling
  draw.bubble(c(agg3$x,0), c(agg3$y,0), c(agg3$z, 2131), maxradius = 0.5, pch = 21, bg = "#00FF0050")
  legend.z2 <- round(max(agg3$z, na.rm = TRUE)/1,0) # tweak divisor for size
  legend.bubble("bottomright", z = legend.z2, maxradius = 0.5, inset = 0.02,
                bg = "lightblue", txt.cex = 0.8, pch = 21, pt.bg = "#00FF0050", n = 8)
}
# good. Export legend from 2003 (max) then turn off legends

par(mfrow = c(6,4)) # set matrix
par(mar = c(0, 0, 1, 0), # margin space buffer around plot
    oma = c(0, 0, 0, 0),
    mgp = c(0, 0, 0),
    xaxt = "n",
    yaxt = "n") # dist of lat/long labels,markers,line from axis

for (i in years) {
  forCPUE <- CPUE2[CPUE2$Year == i,]
  agg3 <- aggregate(list(z = forCPUE$TotalNo),
                    list(x = forCPUE$Midpoint.Lon, y = forCPUE$Midpoint.Lat), sum)
  basemap(xlim3, ylim3, main = i, xlab = "", ylab = "")
  draw.shape(coast, col = "cornsilk")
  # zeroes and maxyearmax added to put the max value offscreen to bind max values for scaling
  draw.bubble(c(agg3$x,0), c(agg3$y,0), c(agg3$z, 2131), maxradius = 0.5, pch = 21, bg = "#00FF0050")
  #legend.z2 <- round(max(agg3$z, na.rm = TRUE)/1,0) # tweak divisor for size
  #legend.bubble("bottomright", z = legend.z2, maxradius = 0.5, inset = 0.02,
  #              bg = "lightblue", txt.cex = 0.8, pch = 21, pt.bg = "#00FF0050", n = 8)
}
basemap(xlim3, ylim3, main = "", xlab = "", ylab = "", bg = "lightblue")
legend("center", legend = c(100, 500, 1000, 2131), pch = 21, col = "black", pt.bg = "#00FF0050",
       bg = "lightblue", y.intersp = 2.1, x.intersp = 2, bty = "n",
       pt.cex = c(100/325, 500/325, 1000/325, 2131/325))
# legend.bubble("center", z = 2131, maxradius = 0.5, inset = 0.02,
#              bg = "lightblue", txt.cex = 0.6, pch = 21, pt.bg = "#00FF0050",
#              n = 3)

