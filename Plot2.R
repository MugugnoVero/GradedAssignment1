#-----------------------------------------#
#         Peer graded assignment.         #
#-----------------------------------------#

#### Load libraries and prepare session. ####

library(RMugugno)

link = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

handle_remote_file = function(x, setDT = TRUE){
  destdir = tempdir()
  dfile = tempfile(tmpdir = destdir, fileext = ".zip")
  download.file(x, destfile = dfile)
  unzip(dfile, exdir = destdir)
  file = list.files(destdir, full.names = T, pattern = ".txt")
  res = read.csv2(file, na.strings = "?")
  if(setDT){
    setDT(res)
  }
  return(res)
}

# f = cache_function(handle_remote_file, 'second_assignment', force = T, folder = conf$default_temp_folder)
f = cache_function(handle_remote_file, 'second_assignment', force = F, folder = conf$default_temp_folder)
res = f(link)

# Convert.
res[, `:=` (
  Date = as.Date(Date, format = "%d/%m/%Y"),
  Time = strptime(paste(as.Date(Date, format = "%d/%m/%Y"), Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
)]

# Subset.
res = res[Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"), list(
  Date,
  Time,
  Global_active_power = as.numeric(Global_active_power),
  Global_reactive_power = as.numeric(Global_reactive_power),
  Voltage = as.numeric(Voltage),
  Global_intensity = as.numeric(Global_intensity),
  Sub_metering_1 = as.numeric(Sub_metering_1),
  Sub_metering_2 = as.numeric(Sub_metering_2),
  Sub_metering_3 = as.numeric(Sub_metering_3)
)]

# Plot 2
with(data = res, {
  png(filename = "C:/Users/Andrea/Desktop/Plot2.png")
  plot(Time, Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)")
  dev.off()
})
