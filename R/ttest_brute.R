# T-tests for all pairs of 2 groups, multiple variables, multiple regions####
# Returns csvs of p values, significance of p values, n (by group & pooled), t values, df,
# group means & SDs, shapiro wilk test for normality p value & logical is.normal result
# see /home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/ for outputs

# Params####
data = AllDailies # x
groupname1 = "GOM"
groupname2 = "Med"
saveloc = "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/"
# Are means different, GOM vs Med stocks?
# Areas: GSL, ForageW, MixingW, MixingWhotspot, GOM vs Med SGs
# Variables: Mean Depth, Max Depth, OceanDepth, Mean Temp, Min Temp, Li5day, Eddy speed, cyc amp, combo?,
myareas <- c("GSL", "ForageW", "MixingW", "MixingWhotspot", "GOM_Med", "Keys_SOG", "Keys_Other", "SOG_Other")
myvariables <- c("MeanDepth24h", "MaxDepth24h", "OceanDepth", "MeanETemp24h", "MinETemp24h", "li5day", "speed_average", "cyclonicAmp",
                 "EddySpeedAmp", "DistanceToShoreKm", "lat", "Hrs50mLesDepRange", "SurfFreq24h", "StepLengthBL", "ild_dive_cnt_desc_gl_Sum")

resultsDF <- as.data.frame(matrix(nrow = length(myvariables),
                                  ncol = length(myareas),
                                  dimnames = list(myvariables, myareas)))
resultsDFt <- resultsDF
resultsDFdf <- resultsDF
resultsDFsig <- resultsDF
resultsDFn <- resultsDF

shapiroDF <- c(paste0(myareas, "_GOM"),
               paste0(myareas, "_Med"))
shapiroDF <- shapiroDF[order(c(seq_along(1:(length(shapiroDF) / 2)), seq_along(1:(length(shapiroDF) / 2))))]
# https://stackoverflow.com/questions/16443260/interleave-lists-in-r/33882030
shapiroDF <- as.data.frame(matrix(nrow = length(myvariables),
                                  ncol = length(shapiroDF),
                                  dimnames = list(myvariables, shapiroDF)))
shapiroNormal <- shapiroDF
shapiroN <- shapiroDF
resultsMeans <- shapiroDF
resultsSDs <- shapiroDF

# tibble's don't collapse into a vector, instead an X x 1 df, which breaks various functionality.
# if ("tbl" %in% class(AllDailies)) AllDailies <- as.data.frame(AllDailies)
# tbl needed for pull()

for (i in myareas) { # i <- myareas[1]
  for (j in myvariables) { # j <- myvariables[1]
    i2 <- i
    if (i == "GOM_Med") i2 <- c("GOM", "Med")
    myx <- AllDailies[which(AllDailies$MarineZone %in% i2 & AllDailies$Stock == "GOM"), j]
    myy <- AllDailies[which(AllDailies$MarineZone %in% i2 & AllDailies$Stock == "Med"), j]

    if (i == "MixingWhotspot") {
      myx <- AllDailies[which(between(AllDailies$lon, -79, -73) & between(AllDailies$lat, 31, 36) & AllDailies$Stock == "GOM"), j]
      myy <- AllDailies[which(between(AllDailies$lon, -79, -73) & between(AllDailies$lat, 31, 36) & AllDailies$Stock == "Med"), j]
    }

    if (i == "Keys_SOG") {
      myx <- AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5) & AllDailies$Stock == "GOM"), j] # keys
      myy <- AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37) & AllDailies$Stock == "Med"), j] # SOG
    }

    if (i == "Keys_Other") {
      myx <- AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5) & AllDailies$Stock == "GOM"), j] # keys
      myy <- AllDailies[which(!(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5) & AllDailies$Stock == "GOM")), j] # not keys
    }

    if (i == "SOG_Other") {
      myx <- AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37) & AllDailies$Stock == "Med"), j] # SOG
      myy <- AllDailies[which(!(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37) & AllDailies$Stock == "Med")), j] # not keys
    }

    if (all(is.na(myx)) | all(is.na(myy))) next # if either myx or myy are all NA, skip to next j

    tmp <- t.test(x = myx,
                  y = myy,
                  alternative = "two.sided",
                  mu = 0, # difference in means
                  paired = FALSE,
                  var.equal = FALSE, # default. Uses Welch's t-test
                  conf.level = 0.95) # default
    resultsDF[j, i] <- tmp$p.value
    resultsDFt[j, i] <- tmp$statistic[[1]] # t statistic
    resultsDFdf[j, i] <- tmp$parameter[[1]] # degrees of freedom
    if (between(tmp$p.value, 0.01, 0.05)) resultsDFsig[j, i] <- "*" # <0.05
    if (between(tmp$p.value, 0.001, 0.01)) resultsDFsig[j, i] <- "**" # < 0.01
    if (tmp$p.value < 0.001) resultsDFsig[j, i] <- "***" # < 0.001
    rm(tmp) # prevents using this value in future loops in case there's an error for that future loop

    # report results in text, per https://www.scribbr.com/statistics/t-test/
    # save this somewhere
    paste0("The difference in ",
           j, # variable name
           " between ",
           "GOM", # group name 1, to be param in function
           " (Mean = ",
           1.46, # group 1 mean
           ", SD = ",
           0.206, # group 1 sd
           ") and ",
           "Med", # group name 2, to be param in function
           " (Mean = ",
           5.54, # group 2 mean
           ", SD = ",
           0.569, # group 2 sd
           ") was ",
           if (tmp3 >= 0.05) "not ", # significant / not significant
           "significant (t = ",
           round(tmp$statistic[[1]], digits = 4),  # t statistic
           ", p < ",
           round(tmp$p.value, digits = 4), # p statistic
           ", df = ",
           round(tmp$parameter[[1]], digits = 0), # degrees of freedom
           ")."
    )

    myx <- myx %>% pull # removes vector from tibble
    myx <- myx[!is.na(myx)]
    if (length(myx) > 5000) myx <- sample(x = myx, size = 5000) # sample size must be between 3 and 5000
    shapiro <- shapiro.test(myx)
    shapiroDF[j, paste0(i, "_GOM")] <- shapiro$p.value
    shapiroNormal[j, paste0(i, "_GOM")] <- ifelse(shapiro$p.value > 0.05, TRUE, FALSE) # p-value > 0.05 = is normal

    ggpubr::ggqqplot(data = myx, title = paste(paste0("Region: ", i),
                                               paste0("Variable: ", j),
                                               "Stock: GOM", sep = "\n")) +
      ggsave(paste0(saveloc, today(), "_QQPlot_", i, "_", j, "_GOM.png"), # WATL TAG FILTER _WatlTagged
             plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
             height = 4/2, units = "in", dpi = 300, limitsize = TRUE)

    myy <- myy %>% pull
    myy <- myy[!is.na(myy)]
    if (length(myy) > 5000) myy <- sample(x = myy, size = 5000)
    shapiro <- shapiro.test(myy)
    # shapiro$p.value # 3.290644e-66
    shapiroDF[j, paste0(i, "_Med")] <- shapiro$p.value
    shapiroNormal[j, paste0(i, "_Med")] <- ifelse(shapiro$p.value > 0.05, TRUE, FALSE) # p-value > 0.05 = is normal

    ggpubr::ggqqplot(data = myy, title = paste(paste0("Region: ", i),
                                               paste0("Variable: ", j),
                                               "Stock: Med", sep = "\n")) +
      ggsave(paste0(saveloc, today(), "_QQPlot_", i, "_", j, "_Med.png"), # WATL TAG FILTER _WatlTagged
             plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
             height = 4/2, units = "in", dpi = 300, limitsize = TRUE)

    resultsDFn[j, i] <- length(myx) + length(myy)
    shapiroN[j, paste0(i, "_GOM")] <- length(myx)
    shapiroN[j, paste0(i, "_Med")] <- length(myy)
    resultsMeans[j, paste0(i, "_GOM")] <- mean(myx, na.rm = TRUE)
    resultsMeans[j, paste0(i, "_Med")] <- mean(myy, na.rm = TRUE)
    resultsSDs[j, paste0(i, "_GOM")] <- sd(myx, na.rm = TRUE)
    resultsSDs[j, paste0(i, "_Med")] <- sd(myy, na.rm = TRUE)

  } # close j myvariables
} #  close i myareas
write.csv(resultsDF, paste0(saveloc, "ttestPvalues.csv"))
write.csv(resultsDFt, paste0(saveloc, "ttestTvalues.csv"))
write.csv(resultsDFdf, paste0(saveloc, "ttestDFvalues.csv"))
write.csv(resultsDFsig, paste0(saveloc, "ttestPvaluesSig.csv"))
write.csv(shapiroDF, paste0(saveloc, "shapiroDF.csv"))
write.csv(shapiroNormal, paste0(saveloc, "shapiroNormal.csv"))
write.csv(resultsDFn, paste0(saveloc, "ttestTotalN.csv"))
write.csv(shapiroN, paste0(saveloc, "ttestGroupNs.csv"))
write.csv(resultsMeans, paste0(saveloc, "groupMeans.csv"))
write.csv(resultsSDs, paste0(saveloc, "groupSDs.csv"))

# Join all these together somehow?
