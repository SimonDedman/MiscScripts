#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simon Dedman simondedman@gmail.com started 2019.06.13
# Please credit any chared work in publications, inc github.com/SimonDedman
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#8 Spike dive, sunrise/sunset####

detach("package:dplyr", unload = TRUE)
library(lubridate)
library(beepr)
library(foreach)
library(doMC)
library(progress)
source('~/Dropbox/Galway/Analysis/R/daylength/daylength.R')
options(error = function() beep(9))  # give warning noise if it fails


df_i <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object
print(paste0(which(loadlist == i), " of ", length(loadlist), "; adding spikedive data to ", i))

dfidates <- aggregate(df_i[,c("lat", "lon")], #just date lat lon
                      by = list(df_i$Date),
                      mean, na.rm = T)
colnames(dfidates)[1] <- "Date"
dfidates$lat[is.nan(dfidates$lat)] <- NA #convert NaN to NA
dfidates$lon[is.nan(dfidates$lon)] <- NA #convert NaN to NA
dfidates <- dfidates[!is.na(dfidates$lat),] # omit rows with NA values for lat
# quicker to call daylength() once per df_i then lookup against that dl than call
# daylength() every morning and evening for each date
dl <- daylength(date = dfidates$Date,
                lat = dfidates$lat,
                lon = dfidates$lon,
                tzs = "America/New_York")
#dl has date lat lon sunrise sunset dawn dusk daylength, mostly "POSIXct" "POSIXt"

df_i$dawn <- rep(NA, nrow(df_i))
df_i$dawn <- as.POSIXct(df_i$dawn, tz = "America/New_York")
attr(df_i$dawn, "tzone") <- "EST"
df_i$sunrise <- rep(NA, nrow(df_i))
df_i$sunrise <- as.POSIXct(df_i$sunrise, tz = "America/New_York")
attr(df_i$sunrise, "tzone") <- "EST"
df_i$sunset <- rep(NA, nrow(df_i))
df_i$sunset <- as.POSIXct(df_i$sunset, tz = "America/New_York")
attr(df_i$sunset, "tzone") <- "EST"
df_i$dusk <- rep(NA, nrow(df_i))
df_i$dusk <- as.POSIXct(df_i$dusk, tz = "America/New_York")
attr(df_i$dusk, "tzone") <- "EST"
df_i$daylength <- rep(NA, nrow(df_i))
df_i$isday <- rep(NA, nrow(df_i)) #changed to NA from FALSE, else gives false when there's no latlon
is.na(df_i) <- do.call(cbind,lapply(df_i, is.infinite)) #convert Inf & -Inf to NA

k <- 1 # set counter

for (j in dfidates$Date) { #loop through dfidates so as to ignore missing latlons?
  #successive row blocks of same day #j <- dfidates$Date[1]
  print(paste0(k, " of ", length(dfidates$Date), "; ", as.Date(j, origin = "1970-01-01"), "; ", i, "; ", which(loadlist == i), " of ", length(loadlist)))

  df_i[df_i$Date == j,"SpikeDives"][1] <- length(which(c(dawnspikeTF,duskspikeTF))) # N spikes per day, append to once-per-day date row
  df_i[df_i$Date == j,"sunrise"][1] <- dl[dl$date == j,"sunrise"] #daily sunrise times etc
  df_i[df_i$Date == j,"sunset"][1] <- dl[dl$date == j,"sunset"]
  df_i[df_i$Date == j,"dawn"][1] <- dl[dl$date == j,"dawn"]
  df_i[df_i$Date == j,"dusk"][1] <- dl[dl$date == j,"dusk"]
  df_i[df_i$Date == j,"daylength"][1] <- dl[dl$date == j,"daylength"]

  # isday append
  daytime <- interval(df_i[df_i$Date == j,"dawn"][1], #from dawn
                      df_i[df_i$Date == j,"dusk"][1]) #to dusk
  df_i[df_i$Date == j & df_i$DateTimeUTCmin5 %within% daytime, "isday"] <- TRUE #mark days for later
  df_i[df_i$Date == j & !df_i$DateTimeUTCmin5 %within% daytime, "isday"] <- FALSE #mark nights for later

  k <- k + 1 #counter, using j directly causes problems due to nightmarish handling of dates in R
} #close j

saveRDS(object = df_i, file = paste0(saveloc,i))
rm(list = ls()) #remove all objects
beep(8) #notify completion
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
gc() #garbage collection, free up memory
