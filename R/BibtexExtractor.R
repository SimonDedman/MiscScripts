# (Qiqqa) Bibtex Extractor, turns .bib files into character list of the refs only
# Simon Dedman simondedman@gmail.com 2020.01.30

bibextract <- function(x = "~/Dropbox/TRANSFER/QiqqaLibraryFI.bib") { #x is character string of location of .bib file including extension
  library(stringr)
  mybib <- read.csv(x, header=FALSE, comment.char="%")
  mybib$V1 <- as.character(mybib$V1)
  # unique(mybib$V1)
  atrows <- which(!is.na(str_match(mybib$V1, "@")))
  atlines <- mybib$V1[atrows]
  # @article
  # @book
  # @inbook
  # @misc
  # @phdthesis
  # @techreport
  # remove @comment
  atrows <- which(is.na(str_match(atlines, "@comment"))) #we want NAs i.e. not @comments
  atlines <- atlines[atrows]
  atlinessplit <- str_split(atlines, pattern = "\\{")
  refslist <- sapply(atlinessplit,`[`,2)
  return(refslist)
} # close function

# FI <- bibextract("~/Dropbox/TRANSFER/QiqqaLibraryFI.bib")
# All <- bibextract("~/Dropbox/TRANSFER/QiqqaLibraryAll.bib")
# FInotinAll <- FI[which(!FI %in% All)]
