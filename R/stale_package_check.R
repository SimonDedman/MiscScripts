# Are loaded packages used?
# https://github.com/MichaelChirico/funchir/blob/master/R/utils.R
# usage: setwd("R script directory), stale_package_check('script.R')
stale_package_check = function(con) {
  code = readLines(con = con)
  #should fix this to match require calls too...
  pkg_used =
    #also misses lines with multiple package calls
    gsub('.*library\\(["]?([^)]*)["]?\\).*', '\\1',
         grep('library', code, value = TRUE, fixed = TRUE))
  for (pkg in unique(pkg_used)) {
    fns = getNamespaceExports(pkg)
    # short functions like D may match even if unused as a function
    # this check could be more sophisticated
    used = names(which(sapply(fns, function(x)
      any(grepl(x, code, fixed = TRUE)))))
    if (length(used)) cat('\nFunctions matched from package ', pkg, ': ',
                          paste(used, collapse = ', '), sep = '')
    else cat('\n**No exported functions matched from ', pkg, '**', sep = '')
  }
}
