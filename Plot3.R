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
#### Plot 3. ####
library(ggplot2)

tmp = as.data.table(summarySCCPM25[summarySCCPM25$fips == "24510", c('Emissions', 'year', 'type')])
data = tmp[, list(Emissions = sum(Emissions, na.rm = T)), by = list(year, type)]

with(data = as.data.frame(data), {
  g = ggplot(data, aes(x = year, y = Emissions, color = factor(type))) #, shape = factor(type)
  g = g + geom_point(size = 3) + geom_smooth()
  g + labs(x = "Year", y = "Emissions", title = "PM2.5 emissions by type - tonnes.", colour = "Type")
  ggsave(filename = "C:/Users/Andrea/Desktop/Plot3.png")
})
