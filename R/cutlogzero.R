# cutlogzero function cuts a vector ensuring it starts with 0, and labels succinctly based on data
# assumes you want breaks of 0:nbreaks rising in ones
# can accept logged data and return the unlogged labels
# Simon Dedman, simondedman@gmail.com, 2022-12-12
# note to self: used in /home/simon/Dropbox/FIU/FIUSharkEcol/R/EROS3_2_traitsPanel.R

cutlogzero <- function(
    x = NULL, # numeric vector
    nbreaks = 10, # integer of breaks desired
    unloglabs = FALSE # if x are logged, give the unlogged equivalents for labels
){
  nbreaks <- nbreaks - 2 # adds Infs at ends
  x <- cut(x = x,
           breaks = c(-Inf, 0:nbreaks, Inf),
           if (unloglabs) {
             labels = c(as.character(0),
                        paste0("<=", round(expm1(1:nbreaks))), # labels in rounded log scale
                        paste0(">", round(expm1(nbreaks))))
           } else { # if not unloglabs, i.e. x are not logged, then:
             labels = c(as.character(0),
                        paste0("<=", 1:nbreaks), # labels in rounded log scale
                        paste0(">", nbreaks))
           } # close ifelse
  ) # close cut
  return(x) # returns a factor
}
