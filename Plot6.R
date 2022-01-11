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

#### Plot 6. ####
carsSources = subset(SourceClassificationCode, subset = EI.Sector %like% 'Vehicles')
rawSubs = subset(summarySCCPM25, fips %in% c("24510", "06037"))
mapping = data.frame(city = c("Baltimore", "Los Angeles"), fips = c("24510", "06037"))
rawSubs = merge(rawSubs, carsSources, by = "SCC", all = F)
table(rawSubs$fips)
emissionsWithOrigin = merge(rawSubs, mapping, on = 'fips')
head(emissionsWithOrigin)
table(emissionsWithOrigin$fips, emissionsWithOrigin$city)

with(data = emissionsWithOrigin, {
  png(filename = "C:/Users/Andrea/Desktop/Plot6.png")
  tmp = tapply(Emissions, list(city, year), sum)
  tmpBalt = tmp["Baltimore", ]
  tmpLA = tmp["Los Angeles", ]
  xVals = names(tmpBalt)
  ylims = range(tmpBalt, tmpLA)
  par(mfrow = c(1, 2))
  plot(x = xVals, y = tmpBalt, pch = 16, col = 'red',# ylim = ylims,
    xlab = "Year", ylab = "PM2.5 emissions - tonnes/year", main = "PM2.5 emissions - Baltimore.")
  #points(x = xVals, y = tmpLA, pch = 17, cex = 1, col = 'green')
  plot(x = xVals, y = tmpLA, pch = 17, cex = 1, col = 'green',# ylim = ylims,
    xlab = "Year", ylab = "PM2.5 emissions - tonnes/year", main = "PM2.5 emissions - LA.")
  dev.off()
})
