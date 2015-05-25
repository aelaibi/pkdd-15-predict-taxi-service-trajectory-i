#USAGE
#
#FUNCTION PARAMETERS: @filename
#@submission: path+filename containing trajectories in CSV format
process.data<-function(filename)
{
	libraries()
	dt<-read.csv2(filename,sep=",")

	dt$CALL_TYPE<-as.character(dt$CALL_TYPE)
	dt$DAY_TYPE<-as.character(dt$DAY_TYPE)
	dt$MISSING_DATA<-as.character(dt$MISSING_DATA)
	dt$POLYLINE<-as.character(dt$POLYLINE)
	str(dt)
	print(unique(dt$MISSING_DATA))

	my_poly<-dt$POLYLINE[2]
	print(my_poly)
	print("")
	i<-1
  print("loop on polyline")
	for (mp in c(1:length(dt$POLYLINE)))
	{
		print(mp)
		print(miliseconds_to_date(dt$TIMESTAMP[mp]))
		my_poly<-dt$POLYLINE[mp]
		if(nchar(my_poly)>2)
		{
			ab<-unlist(strsplit(my_poly,"[]]"))
			polyline<-c(substr(ab[1],3,nchar(ab[1])),sapply(ab[2:(length(ab)-1)],my.substr))

			lat<-c(as.numeric(sapply(polyline,getlat)))
			lon<-c(as.numeric(sapply(polyline,getlon)))

			create_map(length(dt$POLYLINE),lat,lon)
		}
		print(mp)
		print(miliseconds_to_date(dt$TIMESTAMP[mp]))
		x<-scan()
	}
}

miliseconds_to_date<-function(dt)
{
  mdy<-month.day.year((dt/3600/24))

  dt<-dt%%(3600*24)
  h<-floor(dt/3600)
  dt<-dt-(h*3600)
  m<-floor(dt/60)
  dt<-dt-(m*60)
  s<-round(dt)

  final<-sprintf("%s/%s/%s %s:%s:%s",format_alg(4,mdy$year),format_alg(2,mdy$month),format_alg(2,mdy$day),format_alg(2,h),format_alg(2,m),format_alg(2,s))
  return(final)
}

create_map<-function(ntraj,lat,lon,zoom=14)
{

  #random colors
  #colfunc<-colorRampPalette(c('red', 'blue', 'green'))
  cols <- rev(colorRampPalette(brewer.pal(8, 'RdYlGn'))(100))

  center = c(mean(lat), mean(lon))
  print(sprintf("Zoom: %d",zoom))
  MyMap <- GetMap(center="Porto", zoom=zoom,GRAYSCALE=FALSE,destfile = "MyTile3.png");

  point_size<-1.0

  print(lat)
  print(lon)
  #tmp <- PlotOnStaticMap(MyMap, lat = lat,lon = lon, cex=point_size,pch=20, col=colfunc(length(lat)), add=FALSE, )
  tmp <- PlotOnStaticMap(MyMap, lat = lat,lon = lon, cex=point_size,pch=20, col=cols, add=FALSE, )
  str(tmp)
}

load.lib<-function(libT,l=NULL)
{
	lib.loc <- l
	print(lib.loc)

	if (length(which(installed.packages(lib.loc=lib.loc)[,1]==libT))==0)
	{
		install.packages(libT, lib=lib.loc,repos='http://cran.us.r-project.org')
	}
}

format_alg<-function(i,n)
{
  s<-sprintf("%d",n)
  while(nchar(s)<i)
  {
    s<-sprintf("0%s",s)
  }
  return(s)
}

libraries<-function()
{
	load.lib("RgoogleMaps")
	library(RgoogleMaps)
	load.lib("colorRamps")
	library(colorRamps)
	load.lib("tm")
	library(tm)
	load.lib("chron")
	library(chron)
}

my.substr<-function(ele)
{
	ab<-substr(ele,4,nchar(ele))
	if (substr(ab,1,1)==' ')
		return(substr(ab,3,nchar(ab)))
	return(ab)
}

getlat<-function(ele)
{
	return(as.numeric(unlist(strsplit(ele,"[,]"))[2]))
}

getlon<-function(ele)
{
	return(as.numeric(unlist(strsplit(ele,"[,]"))[1]))
}

filename <- "/Users/Abdelhaq/dev/DataScience/Kaggle/pkdd-15-predict-taxi-service-trajectory-i/data/train-no-missing-data-3.csv"

dt<-read.csv2(filename,sep=",")


dt$LATF<- as.character(dt$LATF)
dt$LONGF<- as.character(dt$LONGF)

dt$LATF<- as.numeric(dt$LATF)
dt$LONGF<- as.numeric(dt$LONGF)

create_map(35, dt$LATF[1:1000], dt$LONGF[1:1000], 13)

for (s in c(1:15)){
  create_map(35, dt$LATF[s], dt$LONGF[s], 13)
  x<-scan()
}

load.lib("RColorBrewer")
library(RColorBrewer)
load.lib("MASS")
library(MASS)
