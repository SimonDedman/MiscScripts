# Checks all R files in folder/package for parsing errors leading to Error in parse(outFile) : unexpected end of input
# Simon Dedman 2020.06.13 simondedman@gmail.com
setwd("/home/simon/Dropbox/Galway/Analysis/R/gbm.auto/R/") # goto package root
allfiles <- list.files()

# i <- allfiles[1]

for (i in allfiles) {
  print(paste0(which(allfiles %in% i), "/", length(allfiles), " : ", i))
  text <- readLines(i)
  src <- srcfile(i)
  parse(text = text, srcfile = src)  # Generates error
}
