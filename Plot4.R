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
if(!all(sapply(c('SourceClassificationCode', 'summarySCCPM25'), exists))){
  list2env(loadfun(url, cache_name = 'ExplDataAssgn2', force = F), envir = globalenv())
}

#### Plot 4. ####
str(SourceClassificationCode)
table(SourceClassificationCode$Option.Group)
coalSources = subset(SourceClassificationCode, subset = SCC.Level.Four %like% 'coal', select = c('SCC', 'SCC.Level.Four'))
emissionsWithOrigin = merge(x = summarySCCPM25, y = coalSources, by = "SCC", all = F)

with(data = emissionsWithOrigin, {
  png(filename = "C:/Users/Andrea/Desktop/Plot4.png")
  total_emissions = tapply(X = Emissions, INDEX = year, sum, na.rm = T)
  years = names(total_emissions)
  total_k_emissions = total_emissions/1000
  yrange = range(0, total_k_emissions)
  plot(x = years, y = total_k_emissions,
    pch = 18, cex = 1.5, col = 'magenta',
    ylim = yrange,
    xlab = "Year", ylab = "PM2.5 emissions", main = "Total US PM2.5 coal-related emissions - KTons."
  )
  abline(h = max(total_k_emissions), col = 'red')
  abline(h = min(total_k_emissions), col = 'green')
  dev.off()
})
