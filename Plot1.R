#-----------------------------------------#
#         Peer graded assignment.         #
#-----------------------------------------#

#### Load libraries and prepare session. ####

library(RMugugno)

loadfun = function(url, cache_name, force, folder = conf$default_temp_folder){
  downloadAssgn = function(url){
    tempdir = tempdir()
    tempfile = tempfile(tmpdir = tempdir, fileext = '.zip')
    download.file(url, destfile = tempfile, method = 'curl')
    unzip(tempfile, exdir = tempdir)
    unlink(tempfile)
    files = list.files(tempdir, pattern = '.rds', full.names = F)
    files = stats::setNames(files, gsub(pattern = '(_|.rds$)', replacement = '', files))
    loginfo("Loading files %s ...", paste(files, sep = " , "))
    resl = lapply(files, function(x, dir = tempdir){
      loginfo("Loading file %s ...", x)
      return(readRDS(file.path(dir, x)))
    })
    return(resl)
  }
  loader = cache_function(downloadAssgn, cache_name = cache_name, force = force, folder = folder)
  return(loader(url))
}

#### Start - cache data for easy access. ####
url = 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
list2env(loadfun(url, cache_name = 'ExplDataAssgn2', force = F), envir = globalenv())
#str(summarySCCPM25)


#### Plot 1. ####
with(data = summarySCCPM25, {
  # Total PM2.5 US emissions.
  png(filename = "C:/Users/Andrea/Desktop/Plot1.png")
  options(scipen = 5L)
  total_emissions_tons = tapply(X = Emissions, INDEX = year, FUN = sum, na.rm = TRUE)
  years = names(total_emissions_tons)
  total_emissions_k_tons = total_emissions_tons/1000
  yLim = range(0, total_emissions_k_tons + 1000)
  plot(
    x = years, y = total_emissions_tons/1000,
    ylim = yLim,
    pch = 20,
    xlab = "Year", ylab = "PM2.5 emissions", main = "Total US PM2.5 emissions - KTons."
  )
  dev.off()
})
