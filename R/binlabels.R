# bin2 wrapper around bin function which returns bin option for a data range
# bin uses cut which uses (1,2] label format which is ugly
# This converts those to  1-2 format
# Could add the option to have user set format
# Simon Dedman 2019.03.13

binlabels <- function(data,
                      nbins = 5,
                      labels = NULL,
                      method = c("length", "content", "clusters"),
                      na.omit = TRUE){

  suppressMessages({#library load lots of functions silently at the start. Froeschke code
    #library(devtools)
    #install_github("vonjd/OneR")
    library(OneR)
    library(stringr)
  })

  #if na.omit set to FALSE but data has no NAs, change na.omit to TRUE else bin & label lengths won't match
  if (!na.omit) if (!anyNA(data)) na.omit = TRUE
  if (!na.omit) nbins <- nbins - 1 #if naomit false, reduce bin size so NA has a bin

  testbins <- bin(data,
                  nbins = nbins, #but only for label creation stage, NA will be added
                  labels = labels,
                  method = method,
                  na.omit = na.omit) #run bin with no labels

  binlabs <- #character vector of nice labels for ranges
    testbins %>% #of testbins (the bin ranges result with data)
    levels() %>% # get the levels - character string of labels
    str_remove_all("\\(" ) %>% #text string remove (
    str_remove_all("\\]" )  %>% #text string remove ]
    str_replace_all(",", "\\-") #text string replace , with -

  if (!na.omit) nbins <- nbins + 1 #if naomit false, regrow bin size so bin size matches labels size

  goodbins <- bin(data, #re-run bins with specified labels & original params
                  nbins = nbins, #original userset binnumber
                  labels = binlabs, #labels will include NA if specified
                  method = method,
                  na.omit = na.omit)
  goodbins #return bin result
}
