# pieterjanvc 2019.07.12
# https://community.rstudio.com/t/paste-text-as-wrapped-comment/34961/4
# convert clipboard text to comment once pasted into R after running
# Linux requires xclip, typically: sudo apt install xclip
# Steps:
# 1. Load function
# 2. Copy external text to clipboard
# 3. Run textToComment() function (result saved to clipboard)
# 4. Paste result into in R

# install.packages("clipr")
library("clipr")
library("stringr")

textToComment = function(blockWidth = 80){
  myText = read_clip()
  myLines = list()
  while (nchar(myText) > 0){
    myBreak = c(str_locate_all(myText, " ")[[1]][,1], nchar(myText))

    if (length(myBreak) > 0) {
      myBreak = myBreak[myBreak < blockWidth]
      myBreak = myBreak[length(myBreak)]

      myLine = substr(myText, 1, myBreak)
      myText = substr(myText, (myBreak + 1), nchar(myText))
    } else {
      myLine = myText
      myText = ""
    }
    myLines = append(myLines, myLine)
  }
  write_clip(paste(paste("#", unlist(myLines), sep = " ")))
}



# make addin:
# http://rstudio.github.io/rstudioaddins/?version=1.2.1511&mode=desktop
# install.packages("rstudioapi", type = "source")
library("rstudioapi")
source('~/Dropbox/Galway/Analysis/R/My Misc Scripts/pasteComment.R')
# textToComment()
# change last line:
insertText(location, text, id = NULL)
insertText(location,
           text = paste(paste("#", unlist(myLines), sep = " ")),
           id = NULL)
