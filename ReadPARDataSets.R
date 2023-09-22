
#parker::GetIndata('Ebble_PAR_CE1_2013_01_03', 'PAR.data')

MakeDailyData <- function(indata = Ebble_PAR_CE1_2013_01_03)
	{
	library (plyr)
	library (zoo)
	
	indata <- as.data.frame(indata)
	indata <- indata[indata$daynight == 'day',]
	
	#indata <- indata[ !is.na(indata$date),]
	
	#use embed function to calculate differences between observations
	#time_diff <- embed(indata$date, 2)
	#print (table (time_diff <- time_diff[,2] - time_diff[,1]))
	#indata <- as.zoo(indata, indata$date)
	#indata <- indata[ order(indata$date1),]

	outdata <-  ddply (indata[c ( 'Temp', 'Lux','date1')], .(date1), summarize, max.Temp = max(get ('Temp')), mean.Temp = mean ( get('Temp')), min.Temp = min (get('Temp')), median.Temp = median (get('Temp')),
	max.Lux = max(get ('Lux')), mean.Lux = mean ( get('Lux')), min.Lux = min (get('Lux')), median.Lux = median (get('Lux')))

	#outdata1 <-  ddply (indata[c ( 'Lux','date1')], .(date1), summarize, max.Lux = max(get ('Lux')), mean.Lux = mean ( get('Lux')), min.Lux = min (get('Lux')), median.Lux = median (get('Lux')))

	outdata$diffTemp <- outdata$mean.Temp - outdata$median.Temp
	outdata$diffLux <- outdata$mean.Lux - outdata$median.Lux
	
	return (outdata)
	}



#x1 <- parker::GetIndata('Ebble_PAR_CE1_2013_01_03', 'PAR.data')
#x1_daily <- MakeDailyData (x1)
#with (x1_daily, plot (date1, diffTemp))
#with (x1_daily, plot (date1, mean.Temp))
#with (x1_daily, plot (date1, mean.Lux))
#with (x1_daily, plot (date1, median.Lux))
#with (x1_daily, plot (date1, min.Lux))
#with (x1_daily, plot (date1, max.Lux))
#with (x1_daily, plot (date1, mean.Temp))


#ReadPARDataSets()

#with (Ebble_PAR, plot (date, Lux))
#Ebble_PAR$Sunrise <- Sunrise (Ebble_PAR$date)





















































































#Ewan's data
ReadPARDataSets <- function ( grep1 = 'EB', grep2  = 'gnumeric', SiteCode = 'CE1')
	{
	
	#outname = 'Ebble_PAR_2013_01_03',
	library(st)
	data(st)

	library(avon)
	data (SiteLocations)

	library(data.table)
	library(date)
	library(gnumeric)
	library(lubridate)
	library(parker)
	library(O2)
	library(rovelli.functions)
	library(sun)


	lon <- LonLatSite (SiteCode)['lon']
	lat <- LonLatSite (SiteCode)['lat']

	rivername = SiteLocations$River[SiteLocations$SiteCode %in%  SiteCode]



	setwd(dirorig)
	filelist <- list.files ()


	x1 <- grep (grep1, filelist)
	x2 <- grep (grep2, filelist)
	print (x1[x1 %in% x2])
	filelist1 <- filelist[x1[x1 %in% x2]]
	objectnames <- gsub ('[.]gnumeric', '', filelist1)


	for ( i in 1:length(filelist1))
	#for (i in 1)
		{
		c_name <- objectnames[i]
		filename = paste ( c_name, '.gnumeric', sep = '')
		sheetname = paste ( c_name, '.csv', sep = '')

		indata <- read.gnumeric.sheet(file = filename, head=TRUE, sheet.name=sheetname, top.left='A1', stringsAsFactors = FALSE)
		print(head(indata,2))
		date_1 <- trim (as.character (indata$date))

		#x333 <- parse_date_time(date_1, c("%d%m%y %H%M%S %p", "%0d%0m%y %H%M%S %p",  "%d%m%y %H%M%S %p", "%0d%0m%y %H%M%S %p", "%m/%d/%Y %H:%M:%S %p"))
		x333 <- parse_date_time(date_1, c("%m/%d/%Y %I:%M:%S %p", "%Y/%m/%d %H:%M:%S"))
		
		indata$datechar <- date_1
		indata$date <- x333
		assign (c_name, indata)
		x_ndx <- is.na(x333)
		print (c_name)
		print (indata[x_ndx,])
		}


	x2 <- do.call(mget, list (objectnames))
	x3 <- rbindlist (x2)

	outdata <- x3[order (x3$date),]
	outdata$date1 <- as.Date (outdata$date)

	start_date <- min (outdata$date1, na.rm = TRUE)

	
	sunrise <- Sunrise ( outdata$date, lon, lat, uniq = FALSE )
	sunset <- Sunset ( outdata$date, lon, lat, uniq = FALSE )

	outdata$daynight <- 'night'
	outdata$daynight[outdata$date > sunrise & outdata$date < sunset] <- 'day'

	
	c_startdate <- gsub ('-', '_', start_date)

	outname <- paste ( rivername,'_PAR_',SiteCode, '_', c_startdate, sep = '')
	assign (outname, outdata)
	
	
	#setwd(dirdmp)
	save ( list = c(outname), file =  paste (tempdir(), outname, '.rda', sep = ''))
	try (save ( list = c(outname), file =  paste (dirdmp, outname, '.rda', sep = '')))
	
	return (outdata)
	}



#x1 <- ReadPARDataSets()
