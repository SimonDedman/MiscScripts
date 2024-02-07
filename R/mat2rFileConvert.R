# Convert matlab .m files to R files
install.packages("matconv")
library(matconv)
library(stringr)
hMaps <- makeFuncMaps(pathDict = system.file("extdata", "HiebelerDict.txt", package = "matconv"))
source(system.file("extdata", "defDataConv.R", package = "matconv"))
setwd("/home/simon/Dropbox/Blocklab Monterey/Code/HIF Kcal BWhitlock/hif-pbft2_Gitlab/")
whichfiles <- "\\.m"
loadlist <- list.files(pattern = whichfiles)
loadlist <- loadlist[which(str_sub(loadlist, -1, -1) == "m")] #remove .md files etc
dir.create("mat2rConverted")
for (i in loadlist) {
  out <- mat2r(i, funcConverters = hMaps, dataConverters = dataConvs, verbose = 1)
  str_sub(i, -1, -1) <- "R"
  rout <- out$rCode
  save(rout, file = paste0("mat2rConverted/", i))
}

# For file 2, get_HIFs.m:

# Error in gregexpr(paste0("\\", leftChar), lin) :
#   invalid regular expression '\', reason 'Trailing backslash'
# > traceback()
# 6: gregexpr(paste0("\\", leftChar), lin)
# 5: FUN(X[[i]], ...)
# 4: vapply(linesMat[sliceEndSet], function(lin) {
#        ends <- regexpr("end", lin)
#        endInd <- ends + attr(ends, "match.length")
#        rightChar <- substr(lin, endInd, endInd)
#        leftChar <- switch(rightChar, `}` = "{", `)` = "(")
#        rghts <- gregexpr(paste0("\\", rightChar), lin)[[1]]
#        lfts <- gregexpr(paste0("\\", leftChar), lin)[[1]]
#        hier <- findInterval(ends, rghts)
#        endCap <- rev(lfts)[hier + 1]
#        sliceCut <- substr(lin, 1, endCap - 1)
#        varName <- regmatches(sliceCut, regexpr("\\w+$", sliceCut))
#        gsub("end", sprintf("length(%s)", varName), lin)
#    }, "e", USE.NAMES = FALSE)
# 3: convSymbols(linesOut)
# 2: convEasySyntax(codeDes)
# 1: mat2r(i, funcConverters = hMaps, dataConverters = dataConvs,
#        verbose = 1)
