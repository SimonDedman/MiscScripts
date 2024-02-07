#CODE DUMP####
# code snippets here from all working scripts
# organise them
# file as individual scripts
# turn into functions where logical
# chain into multifunctions with pipes where logical
# add functions to function load list when used regularly
# scour C:\Users\simon\Dropbox\Galway\Analysis\R & subfolders

# Headings ####
# 1/word/4 works, also 4 dashes ---
# 4/word/4 works but why bother? Because
##### subheading ####
# 5/word/4 is a subheading!



options(error = function() beep(9))  # give warning noise if it fails, needs beepr package

length(which(hasssm)) #counts TRUE in vec. 398

ssm_eventids <- split(x = ssm, f = ssm$eventid) #   split ssm to per-eventid then apply() maybe lapply()
ssm2 <- do.call("rbind", ssm_eventids) # bind it back together afterwards

caltrawl = merge(blockid2,
                 l_summ_blk_yrs_rept_av_spread,
                 by = "CDFWBlockID",
                 all.x = T)


#Filtering subsetting esp NAs####
df %>% select(cols) %>% dplyr::summarise_all(~ sum(is.na(.) / dplyr::n() * 100)) # what % of selected cols are NA?
tidyr::drop_na(colA, collB, colC) # AllDailies %<>% drop_na(lon, lat)
my_df %>% discard(~all(is.na(.) | . =="")) # tidyverse, drops all NA or all blank
df_nona <- df_i[!is.na(df_i$Date),] # omit rows with NA values for date, downsample to days only
# v[is.nan(v)] <- NA #convert NaN to NA, whole object (doesn't work?)
v$w[is.nan(v$w)] <- NA #convert NaN to NA, column(s) only. Could use v[,c("w", "u")] ?
is.na(ADday) <- do.call(cbind,lapply(ADday, is.nan)) #convert NaN to NA
df[is.na(df)] <- 0 # convert NaN to 0 in data.frame (yes, you use is.na for is.nan)
is.na(dat) <- do.call(cbind,lapply(dat, is.infinite)) #convert inf & -inf to NA
modify_if(AllDailies$WhichForageHours, is.null, ~NA) # replace NULLs with NAs

replaceNA <- function(x, na.rm, ...){
  if (is.na(x[1]))
    return(0)
  else
    return(x)
}
tmp <- mylistofrasters %>% sapply(function(x) raster::calc(x, fun = replaceNA))

rlang::`%||%`(x,y) # x If x is NULL, will return y; otherwise returns x. # library(rlang)
NULL %||% 2 # 2
1 %||% 2 # 1
c(NULL, 1, 2, NULL) %||% 0 # doesn't work for vectors. Need to vectorise with apply or similar.
rlang::`%|%`(x,y) # x If x is NA, will return y; otherwise returns x. # library(rlang)
c(1L, NA, 3L, NA, NA) %|% (6L:10L)
for (i in 1:ncol(x)) {print(paste0(names(x)[i], ": ", any(is.infinite(x))))} #check infinites in df. Fails on tibble
sum(is.na(df$col)) #count NAs in a column
clean_data$PRCP[clean_data$PRCP == -999.9] = NA # replace -999.9 with NA
is.na(df$colname) <- df$colname == "unk" # replace char pattern in df/tbl column with NA
is.na(df) <- df == "foo" # replace char pattern in whole df/tbl with NA
replace_na(data, replace, ...) # replace NA with something, tidyr
myData[-c(2, 4, 6), ] # delete specific rows
# if a character object exists in a character vector, remake the charvec as current charvec indexed by inverse of T/F vec of whether charObj = charvec position N
# i.e. remove a named object from character vector if present
if (paste0("AllDailies_", savename, ".Rds") %in% loadlist) loadlist <- loadlist[!loadlist %in% paste0("AllDailies_", savename, ".Rds")]

x[-1] # is vector except first value
x[-length(x)] # is vector except last value. both work for dfs also just add comma before ] to indicate rows & change length to nrow

df[df$col < 1,] #subsets df rows on col condition BUT will return NA rows in the result if there are NAs in the col. Instead:
df[which(df$col < 1),] #indexes the rows which fit the condition and returns a subset of those. Otherwise the subsetting index is
# a vector of TRUE/FALSE but the NA rows will be NA and thus deliver all-NA rows to the result.

rownames(df_i) <- NULL # reset rownames so subsets have fresh 1:n rownames not the echoes of the main df

# filter for times within an interval
daytime <- interval(df_i[df_i$Date == j,"dawn"][1], #from dawn
                    df_i[df_i$Date == j,"dusk"][1]) #to dusk
df_i[df_i$Date == j & df_i$DateTimeUTCmin5 %within% daytime, "isday"] <- TRUE #mark days for later

df_i <- dplyr::arrange(df_i, Date)# order df_i by date, low to high

# filter rows & columns & do actions on those, base R
mean(df[df$b == 2, "a"], na.rm = T)
# dplyr https://stackoverflow.com/questions/58034173/subsetting-rows-and-columns-for-both-data-frame-and-tbl-df
dftbl %>%
  filter(b == 2) %>%
  select(a)  %>%
  .$a %>%
  mean(., na.rm = TRUE)
# also # see last comment, evaluation block means you can't do select(a) %>% mean(.$a, na.rm = TRUE)
dftbl %>%
  filter(b == 2) %>%
  select(a)  %>%
  {mean(., na.rm = TRUE)}

# group by 1 col (Date) & get normal summaries (xstempdaily_mean) & summaries filtered by another col (xstempnight_mean; isday)
# [isday ==FALSE] works identically
df_i %<>% group_by(Date) %>% # per day
  summarise(Index = min(Index, na.rm = T), #create table of Date Index xstempdaily_mean xstempnight_mean
            xstempdaily_mean = mean(ExcessTemp.C., na.rm = T),
            xstempnight_mean = mean(ExcessTemp.C.[!isday], na.rm = T)) %>%
  ungroup()

sf::st_drop_geometry(df) # remove geometry column




#Pipes####
# |> # R 4.1.0 native pipe, ctrl+shift+m shortcut. Doesn't need magrittr/dplyr (which uses magrittr)
  df %<>% mutate(mycolname = as.integer(mycolname)) #change col class in pipe
a <- b <- 1 # never EVER do this. Both a & b will be created in the same memory space and will BE the same forever so whatever you do to one happens to the other
# %$% #magrittr, Expose the names in lhs to the rhs expression. This is useful when functions do not have a built-in data argument.

  summarise(WhichForageHours = list(hour(HourFloor))) # allows vector output to df, bypassing single value result rule

hist(MonthlyDSTs %>%
       filter(Month == 2) %>%
       pull(MonthlyDST)) # pull gets a vector from a tibble & works in pipe, allowing results to be used in normal situations

# make new column as if_else on other column
df %>%
  mutate(new_col = case_when(
    old_col == 0 ~ 2, # oldcolval ~ newcolval
    old_col == 5 ~ 30,
    TRUE ~ old_col * 5)) # TRUE for all other oldcolvals not specified above

# plot & write.csv mid pipe: %T>%
rnorm(200) %>%
  matrix(ncol = 2) %T>%
  plot %>% # plot usually does not return anything.
  colSums %T>% # don't need this second tee pipe since it'll goto write.csv regardless?
  write.csv('name', row.names = F)

# Tee pipe genius:
# https://stackoverflow.com/questions/76443954/how-to-use-magrittr-tee-pipe-t-to-create-multiple-ggplots-for-grouped-data-in
library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)

mtcars |>
  group_by(cyl) %T>% # tee pipe the next command
  # group_walk wants a ~formula
  # print prints the results of its commands, a ggplot chain
  # then %T>% carries on:
  group_walk(~ print( # group_walk works on each sub-tbl group
    ggplot(.) + geom_histogram(aes(x = carb))
  )) |>
  summarise(
    meancarb = mean(carb, na.rm = TRUE),
    sd3 = sd(carb, na.rm = TRUE) * 3
  )

# OR can obviate tee pipe by {bracing} the formula, chaining commands, and returning the tbl with .x
mtcars |>
  group_by(cyl) |>
  group_walk(~ { # brace encloses everything for multi line function
    print(ggplot(.x) + geom_histogram(aes(x = carb)))
    .x # returns tbl
  }) |>
  summarise(
    meancarb = mean(carb, na.rm = TRUE),
    sd3 = sd(carb, na.rm = TRUE) * 3
  )
# can save with:
mtcars |>
  group_by(cyl) |>
  group_walk(~ { # brace encloses everything for multi line function
    p <- ggplot(.x) + geom_histogram(aes(x = carb))
    ggsave(plot = p, filename = paste0(saveloc, lubridate::today(), "_GGhist_", .y$cyl, ".png"))
    .x}) |>
  summarise(meancarb = mean(carb, na.rm = TRUE), sd3 = sd(carb, na.rm = TRUE) * 3)

# Both approaches rely on group_walk using a formula ~ format. For a non-formula-format first command?
mtcars %T>%
  {
    summarise(., sdd = sd(carb)) |>
    print() |>
    write_csv("/home/simon/Desktop/tmp.csv")
  } |>
  summarise(meancarb = mean(carb))


# row number in pipe:
df |> mutate(Index = row_number()) # 1:n
df |> group_by(x) |> n() # current group size

# cbind for dplyr
dplyr::bind_cols()

# replace specific row's values, likst df[df$col == x, "col"] <- newVals, or df[IndexVec, "col"] <- newVals
mutate(geometry = replace(x = geometry, # vector
                          list = datafilt$Index, # index vector list
                          values = data.tmp)) # replacement values

# turn row names to column
mtcars <- mtcars %>% rownames_to_column


# data.table ####
setDT(AllDailies) # convert to data.table without copy
setDT(df_i)
AllDailies[df_i, on = c("Date", "toppid"), MinDepth24h := i.MinDepth24h] # update 1 column's data in place, join on date & toppid, send df_i data to AllDailies
mergecols <- c("MinDepth24h", "MeanDepth24h", "MaxDepth24h", "MaxETemp24h", "MeanETemp24h", "MeanETemp24hU2M", "MinETemp24h", "SurfFreq24h")
AllDailies[df_i, on = c("Date", "toppid"), (mergecols) := mget(paste0("i.", mergecols))] # multiple columns
# https://stackoverflow.com/questions/52838722/looping-assignment-by-reference-through-columns

#folders files & OS####
system("shutdown -h now") # linux shutdown
list.files()
list.dirs() # list files & directories in a folder
if (.Platform$OS.type != "windows") {} #check OS
thesecols <- sapply(df, class) #after read.csv, query colClasses then change them, see TagToTagCbind
# If savedir has a terminal slash, remove it, it's added later
if (substr(x = savedir, start = nchar(savedir), stop = nchar(savedir)) == "/") savedir = substr(x = savedir, start = 1, stop = nchar(savedir) - 1)
# https://stackoverflow.com/questions/9319242/remove-everything-after-space-in-string
sub(" .*", "", x) # Remove everything after space in string
x <- make.names(x) # convert to R legal names e.g. no numerical-named objects.

# run script as compiled function
Name <- function(machine = "/home/simon/Documents/Si Work/") {"all script"}
library(compiler)
cmpfun(Name)
Name() # or DailyDFIsRbind(machine = "/media/Seagate/Work/")


# print class & column details of all objects. Should filter for dataframes only?
# redundant if Rstudio fixes bug https://community.rstudio.com/t/environment-data-frames-expand-with-blue-arrow-but-tbl-dfs-dont/32566/4
allobjects <- objects()
for (i in allobjects) {
  i
  class(get(i))
  str(get(i))
}

within(df, rm(x, y)) # delete df cols by name

library(devtools)
devtools::install_github('jmlondon/ptolemy')
library(ptolemy)
install_gshhg() #single line replacement for gbm.basemap?
extract_gshhg() #function is very much that

future::plan(multiprocess) #multiple processing kinda

colnames(df)[which(colnames(df) == "BadName")] <- "good_name" #replace/clean/rename colnames by name not number reference
x[, ] <- lapply(x[, ], as.character) #change all cols to character
# %>% rename(newName = cyclonioldColNamecAmp.y) #in pipe
mutate(name = str_replace_all(name, c("gom_fish" = "GOM", "med_fish" = "MED", "slopesea_fish" = "SS"))) # find & replace rename in pipe
fishNearEddies %<>% within(rm(cyclonicAmp.x, date.x, date.y, buffer)) #delete columns
df %<>% dplyr::select(-date, -index) # delete columns (needs magrittr?)

new <- A[!A %in% B] # which of 1 isn't in another?

natlantic3 <- gbm.basemap(grids = ssm2, gridslat = 6, gridslon = 7)
natlantic4 <- read_sf("./CroppedMap/Crop_Map.shp")

# auto create nice bin ranges length/content/cluster format
source('C:/Users/simon/Dropbox/Galway/Analysis/R/My Misc Scripts/binlabels.R')
tmp1 <- bin(myvec, nbins = 8, method = "length","content","clusters", na.omit = TRUE)

suppressMessages({ #library load lots of functions silently at the start. Froeschke code
  library(shiny)
  library(shinyBS)
})
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
options(scipen = 5) # prevents plot using 1e+05 godawful notation for 100000. Allows N^10 before Ne+11

# Text strings####
stringr::str_replace_all(my_string, "badcharacter","goodcharacter") #text string replace
stringr::str_remove(string, pattern)
stringr::str_sub(x,-6,-1) #last 6 characters
sprintf("%04d", 104) # pads an input to a number of characters, adding zeroes to the front: "0104"
rm(list = paste0("O2_", o)) # remove algorithmically named objects using the list option in rm
paste(a, collapse = " ") # convert vector into string: a <- c("aa", "bb", "cc")


# cut into specific bins, manual label, create new column with those factors
sf_locs$agefac <- cut(sf_locs$age_years, c(-Inf, 4.999, 8.999, Inf), labels = c("0-4", "5-8", "9+"))


rm(list = c("con", "connectTOPP", "alldbtables")) # remove list
tmp <- as.data.frame(table(tblfg_pdt$eventid)) # get ID & ID frequencies table from a vector, length of vec is length of unique IDs

if (any(grepl(".txt", loadlist))) loadlist <- loadlist[-grep(".txt", loadlist)] #remove textfiles if present

# whole number function. is.integer() doesn't work as expected
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

diff(range(x)) # single value for the range distance, max minus min
# https://stackoverflow.com/questions/56957952/r-range-distance-is-there-an-existing-function

# List combinations:
# for a df (track) with identified (toppid), list all permutations of combinations of another variable (tagtype)
# could make into function easily.
toppids <- unique(track$toppid)
toppidtagtypes <- data.frame(toppids = toppids,
                             tagtypes = rep(NA, length(toppids)))
for (k in toppids) {
  xtmp <- subset(track, subset = toppid == k, select = tagtype)
  toppidtagtypes[which(toppidtagtypes[,1] == k), 2] <- paste(sort(unique(xtmp$tagtype), collapse = " "))
}
unique(toppidtagtypes[,2])

abs(-2) #2 absolute value = modulus

df[order(-df[,"colname"]),] #order df descending by colname. For ascending, remove minus
df[,c(1,2,3,4)] <- df[,c(1,4,2,3)] # reorder columns by index position

data.frame(matrix(NA, nrow = 2, ncol = 3)) # empty df of specific size

for (i in seq(along = object)) #same as 1:length(object), fails better

  # generate a vector of sequences from 2 vectors
  seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to")) #make function
df$seqVec <- seq2(from = df$fromVec, to = df$toVec, by = 1) #send results to column
c(seq2(from = df$fromVec, to = df$toVec, by = 1)) #return results as single vector


findInterval(df_i$lat, sort(unique(do2$Latitude))) #returns vector of indices in sort(unique(do2$Latitude)). Turns second vector into a bin range & finds positions of first vector's data within it



#Lists & purrr####
list2env(mylist, globalenv()) # copy named list objects as named objects into the global environment, overwriting existing objects
EVERYONE <- as.list(.GlobalEnv) # puts everything from the envirnment into a list
sapply(mylist,`[`,2) # extract every second element from a list
# get first item from every items in a nested list: https://stackoverflow.com/questions/44176908/r-list-get-first-item-of-each-element
sapply(mylist, "[[", 1)

# https://stackoverflow.com/questions/71313289/r-transpose-2-matrices-into-a-list-of-tibbles-for-a-nested-df
Map(tibble, lat = asplit(lat, 1), lon = asplit(lon, 1))
purrr::map2(asplit(lat, 1), asplit(lon, 1), ~ tibble(lat=.x, lon=.y))

#convert list to data frame
data.frame(t(sapply(my_list,c)))
# tidyverse verison:
purrr::map_df(mylist, ~.x) # in pipe, remove "mylist, "

# https://stackoverflow.com/questions/72063819/use-an-arrow-assignment-function-as-r-purrr-map2
nameslist <- list("A", "B", "C")
datalist <- list(5, 6, 7)
datalist %<>% setNames(nameslist) # set names of list numbers
names(datalist) # "A" "B" "C"
datalist %<>% purrr::map2(nameslist, ~ .x %>% setNames(.y)) # set names of list objects
names(datalist[[1]]) # A

# Factors ####
droplevels() # drops empty levels


# Dates & times ####
library(anytime)
# anytime::getFormats()
anytime::removeFormats("%m/%d/%Y")
anytime::addFormats("%d/%m/%Y")
finprint$set_date <- anytime::anydate(x = finprint$set_date) # converts character string of excel dates & characters to yyyy-mm-dd date




# Interpolation: linear between multiple points e.g. times####
tointerpol <- c("lat", "lon", "age") #desired column(s) in loadloc files list to interpolate
for (k in tointerpol) { #loop through column options chosen to interpolate by user
  if (all(is.na(df_i[,k]))) next # if all are NA then skip that k & report
  vec <- which(!is.na(df_i[,k])) #vector of row numbers which don't have NA values for desired column
  # IF ALL ARE NA THEN THE JOIN DIDN'T WORK IN MolaExtractedLoadLoop.R
  # in loop this failed for task 69. Feasibly 16 cores means I can work out which in loadlist this was?
  vec1 <- vec[1:(length(vec) - 1)] #that vec except last
  vec2 <- vec[2:(length(vec))] #that vec except first
  vec12df <- data.frame(vec1 = vec1, vec2 = vec2) #df of those two, creating row-pairs of 1st-2nd, 2nd-3rd etc, to interpolate between

  for (j in 1:nrow(vec12df)) { #loop through each rowpair to interpolate
    n <- vec12df[j,2] - vec12df[j,1] + 1 #n is row#2 - row#1 (diff) but +1 as interpolate function includes (given) first & last values we already have
    interpoli <- approx(x = c(df_i[vec12df[j,1], k],
                              df_i[vec12df[j,2], k]),
                        n = n) # interpolate between those values
    interpolivalues <- interpoli$y[2:(length(interpoli$y) - 1)] # remove 1st & last (which are the existing values interpolated between)
    df_i[((vec12df[j,1] + 1):(vec12df[j,2] - 1)) , k] <- interpolivalues # place values in df
  } #close j loop
} # close k loop

month.name[1]

MikeStatus2 <- str_split(stocklist$MikeStatus, pattern = "_") # split into sections by _
stocklist$MikeStatus2 <- sapply(MikeStatus2, head, 1) # get first bit. Copy this to template R file


#Interpolation: fill NA gaps along rows####
do2t <- t(do2) #transpose, na.approx works on columns
do2tData <- do2t[4:nrow(do2t),] # remove other columns, datetime lat lon
do2tDataSpline <- na.approx(do2tData, na.rm = F) #interpolate, don't do terminal NAs (shallows/depths)
do2DataSpline <- t(do2tDataSpline) # transpose back
do2DataSpline2 <- data.frame(do2[,1:3], #as df join datetime lat lon
                             do2DataSpline)
colnames(do2DataSpline2) <- colnames(do2) # rebuild colnames

#Interpolation: Fill holes on XY plane####
# Not done at the time, but see bookmarks Interpolation4D
# Andrew Turpin's natural neighbour code in /Galway/Analysis/R/ but I'm not sure how it works.
# https://github.com/turpinandrew/Natural-Neighbour-Interpolation-in-R/blob/master/naturalNeighbourInterp.r


# Add subgrainMM with QGis
# https://www.qgistutorials.com/en/docs/3/sampling_raster_data.html
# sample raster values
# in future do this in R
# https://github.com/r-spatial/sf/issues/575
# see comments from pat s onwards


# Log exp scaling ####
# create an exponentiating curve rather than a straight line. Used in gbm.auto RSB plots to scale
# can used to have more breaks in the early part of a sequence, eg.
linear01seq <- seq(from = 0, to = 1, length.out = 9) #linear sequence from 0:1, 9 bins
plot(linear01seq,linear01seq)
exp01seq <- expm1(4*linear01seq)/expm1(4) # exponentiate to change shape then scale back to 1
plot(linear01seq,exp01seq)


#spatial####
pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly

# if any longitudes are in 0:360 rather than -180:180 space, convert them
subtracktmp[subtracktmp$lon > 180, "lon"] <- -(360 - subtracktmp[subtracktmp$lon > 180, "lon"])
# make this a function

# after a ggplot
ggsave("Basemap & Blocks.png",
       plot = last_plot(),
       device = "png",
       path = ".",
       scale = 1,
       width = NA,
       height = NA,
       units = c("in", "cm", "mm"),
       dpi = 300,
       limitsize = TRUE)

#make this a function: circle around a point lat lon buffer unit crs
#buffer circle 50 and 100km radii from center
anacapa <- st_point(x = c(-119.425019, 34.011214), dim = "XY")
anacapa <- anacapa %>% st_sfc(crs = 4326)
library(units)
r50 <- set_units(50, km) %>% set_units(km) %>% set_units(m) # define radius of interest, e.g. 50km
anacapa <- st_transform(anacapa, 3310) # change to CRS that uses meters
anacapa_buffer50 <-  st_buffer(anacapa, r50)
anacapa <- st_transform(anacapa, 4326) # eventually convert back to WSG84 if needed for other purposes
anacapa_buffer50 <- st_transform(anacapa_buffer50, 4326)



#Parallel, apply & progressbars####
#Lapply###
race <- c(rep(1, 30), rep(2, 30), rep(3, 40))
age <- rnorm(100, 25, 3)
y <- age * 10 + ifelse(race == 1, 100, ifelse(race == 2, 2000, 0)) + rnorm(100, 0, 1)
racedata <- as.data.frame(cbind(race, age, y))
racedata$race <- as.factor(racedata$race) #100 obs 3 vars race (1:3) age y
lapply(1:3, function(index) summary(lm(y~age, data = racedata[racedata$race == index,])))
# does lm of y~age for racedata$race 1:3
# use sapply instead of lapply if you want the result to be a vector

#Apply###
# apply(dataset, margin, function) #margin: rows (margin=1) or columns (margin=2) of the data
# two cols (since margin=2, i.e. columns) (y and ybin)
# apply function: take argument (outcome) which refers to each of those  columns
# and run lm on each with x and z as the explanatory variables
apply(mydata[,c("y","ybin")], 2, function(outcome){summary(lm(outcome~x+z))})
# WARNING dataset can be 1 row/column but must be indexed as datset[,1] or dataset[,"colA"] NOT dataset$colA
# Since dim(dataset$colA) = NULL whereas dim(theotherways) = n  1

total <- 20
pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
# for (i in 1:total) {   setTxtProgressBar(pb, i) # update progress bar
  #your code
# } close(pb)

library(pbapply)
df_i[(1:lastgood),"OceanDepth"] <- pbapply(df_i[(1:lastgood),c("lon","lat")], #only nonNA rows, only lat lon cols input
                                           1, getz) #on rows, run function


cl <- makeCluster(mycores, outfile = "")
clusterExport(cl = cl,
              varlist = c("lon", "lat", "z"))
df_i[(1:lastgood),"OceanDepth"] <- pbapply(cl = cl, #cluster
                                           df_i[(1:lastgood), c("lon","lat")], #only nonNA rows, only lat lon cols input
                                           1, #on rows
                                           getz) #run function
# Error in serialize(data, node$con) : error writing to connection
# Probably memory limit hit
stopCluster(cl = cl)

# tapply for by-group functions: tapply(data, grouping, function)
tapply(AllDailies$Hrs50mLesDepRange, AllDailies$kmeansBinary, mean)

# Conveniently add progress bar to for-like loops: startpb, setpb, getpb, closepb
# read these. from pbapply help

#dplyr alternative
# https://sebastiansauer.github.io/rowwise_dplyr/
mtcars %>%
  rowwise() %>%
  mutate(mymean = mean(c(cyl,mpg)))
Hourstats %<>% # magrittr
  rowwise() %>%
  mutate(ForagehourA = all(Foragehour,
                           ForageDepth <= 120,
                           ild_depth_gl <= 40))
# allows easy control over FUN's ... conditions and returns the same df with extra cols added




# for if else loops control flow####
# formatting option
if (COND){object <- value1} else {object <- value2}

# dplyr loops
for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()
}
# programming with dplyr vignette, loop over multiple variables: .data[[groupvariable]]
# depreciated in tidyselect 1.2.0, use all_of(groupvariable)
# Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
# Thus use all_of(groupvariable) in select() and .data[[groupvariable]] elsewhere. UGH.




# Symbols meaning####
# %% # modular division: gives the remainder, not the result:
  10 / 10 # 1
10 %% 10 # 0
110 %% 10 # 0
# useful in loops to print progress every nth loop e.g. if (i %% 10 == 0) { #do something}
# https://stackoverflow.com/questions/30257819/what-does-the-double-percentage-sign-mean


# Snippets ####
# snippet ts
`r paste("#", Sys.Date(), "------------------------------\n")`


# Keyboard shortcuts ####
CTRL + SHIFT + R # insert section heading
# <- # Alt minus
  # |> # ctrl shift m
  # ctrl i : indent a line
  # ctrl shift c : comment/uncomment a line
  # ctrl 1 : focus cursor on source pane
  # ctrl 2: focus cursor on console pane (and others for other panes)
  # ctrl shift o : open document outline
  # ctrl down/up : keyboard scroll the active pane
  # ctrl alt down/up : select multiple lines for multicursor
  # ctrl alt shift down/up : don't select lines while shifting, allows noncontiguous multicursor
  # ctrl shift / : reflow long comment to the next line(s)
  # ctrl a : select all
  # ctrl shift a : reformat code nicely. Also addins, style active file.


# AddIns ####
# Restart RStudio after the installation for the addins to load properly.

# Annotater: https://github.com/luisDVA/annotater
# Adds info about the package use, version, and which functions are used, next to library(pkg)
remotes::install_github("luisDVA/annotater")

# https://statsandr.com/blog/rstudio-addins-or-how-to-make-your-coding-life-easier/#addins

# Find & label TODOs
remotes::install_github("dokato/todor")

# Keyboard shortcuts for pipes
remotes::install_github('konradzdeb/extraInserts') # https://github.com/konradzdeb/extraInserts


# plotting ggplot ####
# See Esquisse & ggThemeAssist addins in addins section
ggplot() +   #plot lines by year
  annotation_spatial(sf_zonelines, # sf_points
                     size = 0.3, #line/point thickness on map & legend
                     color = "black", #colour on map & legend
                     key_glyph = "abline") +
  annotation_spatial(natlantic, fill = "grey", lwd = 0) + #layer_spatial trains scales, annotation_ doesnt
  layer_spatial(extents, size = 0, col = "white") +
  annotation_spatial(sfMonth_lines, #  %>% filter(Stock != "UNK", DataType != "P")
                     size = 0.5, #line thickness on map & legend
                     aes(colour = MeanVelBL, #colour on map & legend
                         linetype = Stock),
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  geom_sf_label(data = sfMonth_lines,
                aes(label = month.abb[Month]),
                label.padding = unit(0, "lines"),
                label.r = unit(0, "lines"),
                label.size = 0,
                fill = NA) +
  scale_linetype_manual(values = c("solid", "dashed")) + # add "dotted" for a 3rd group
  scale_colour_gradientn(colours = rev(rainbow(12))) +
  coord_sf(xlim = c(natlanticextents[1,1], natlanticextents[2,1]), # instead of scale_x_continuous, gives white background with rug ticks but no lines
           ylim = c(natlanticextents[1,2], natlanticextents[2,2]),
           expand = F) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.84, 0.1), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggtitle(paste0("Mean daily distance travelled (body lengths) by ABFT"),
          subtitle = paste0("By stock and month, ", datemin, ":", datemax,
                            "; n fish=", length(unique(AllDailies$toppid)),
                            ", GOM: ", AllDailies %>% filter(Stock == "GOM") %>% summarise(toppid = unique(toppid)) %>% nrow,
                            ", Med: ", AllDailies %>% filter(Stock == "Med") %>% summarise(toppid = unique(toppid)) %>% nrow)) + # change sflines
  ggsave(paste0(saveloc, today(), "_MonthlyVelByStock.png"),
         plot = last_plot(), device = "png", path = "",
         scale = 2, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 6, #NA default. Manually adjust plot box in RStudio after ggplot()
         height = 4.5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
         dpi = 300, limitsize = TRUE)


ggplot(AllDailies %>% drop_na(MarineZone_f), # ,MeanDepth24h > 1
       aes(x = as.Date(Day, origin = as.Date("2018-01-01")),
           y = MeanDepth24h)) +
  geom_point(aes(colour = Stock), size = 0.5) + # , col = toppid # , group = toppid
  stat_smooth(aes(colour = Stock)) +
  facet_wrap(.~MarineZone_f) + # , scales = "free" default fixed scales allows sharing axis labels
  labs(x = "Year Day", y = "Mean Depth, m") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE, "grey")) +
  # avoid terminal Jan: https://stackoverflow.com/questions/14759676/specification-of-first-and-last-tick-marks-with-scale-x-date
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2018-01-01"), as.Date("2018-12-30")), expand = c(0,0)) +
  # scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100), trans = recurve_trans) + # depth as positive, 0 at bottom (bad)
  # scale_y_continuous(limits = c(600, 0), breaks = seq(600, 0, by = -100), trans = recurve_trans) + # depth as positive, 0 at top: no data plots, excluding limits plots data but 0bottom, excluding breaks 0top decent breaks no data
  scale_y_continuous(limits = c(-600, 0), breaks = myseq, trans = recurve_trans) + # depth as negative, works
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   # legend.key.width = unit(2, "cm"),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_FacetZone_MeanDepth", ".png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)



ggplot(AllDailies %>% drop_na(MarineZone_f),
       aes(x = MarineZone_f, y = MeanDepth24h, colour = Stock, fill = Stock)) +
  geom_violin() + # , size = 0.5
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, position = position_dodge(0.9), colour = "white") + # , fill = "black" kills stock separation
  # stat_summary(fun.data = mean_sdl, mult = 1, geom = "pointrange", color = "black") +
  labs(x = "Marine Zone", y = "Mean Depth, m") +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  {if (condition) plot_something} + # if conditional in ggplot
  scale_y_continuous(limits = c(-600, 0), breaks = myseq, trans = recurve_trans) # depth as negative, works

df %>%
  operations %>%
  {. ->> tmp} %>% # https://stackoverflow.com/questions/61503696/how-to-refer-to-a-piped-object-inside-ggplot
  ggplot() +
  operations +
  scale_x_continuous(limits = c(min(tmp$Year), max(tmp$Year)),  # can access intermediate/ggplot call object terms
                     breaks = (min(tmp$Year):max(tmp$Year)))

# Superscript
labs(x = expression("Geographic Range" ~ (Million ~ Km^{2})), # https://stackoverflow.com/questions/20980761/superscript-in-r
     y = "Market Gravity")
