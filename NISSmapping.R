#Load libraries
library(maps)
library(mapdata)
library(rworldmap)
library(ggmap)
library(wesanderson)

########get blank worldmap#########
newmap <- getMap(resolution = "high")
########get x and y limits from Scotland limits##########
Scotland.limits <- geocode(c("Out Stack, Shetland",
	"Cairngaan, Wigtownshire",
	"Rockall, Scotland",
	"Bound SKerry, Out Skerries, Shetland Islands")
)
#####Set colour palletes#####
pal <- wes_palette("Zissou", 4, type = "continuous")
pal2 <- wes_palette("FantasticFox", 5, type ="continuous")

######plot worldmap specifically Scotland#######
plot(newmap, 
	xlim = range(Scotland.limits$lon),
	ylim = range(Scotland.limits$lat),
	asp = 2,
	col=pal,
	bg=pal,
)
NSIS <- read.csv("waypoints.csv")

points(NSIS$LAT,NSIS$LON,col=pal2,cex=1.5,pch=17)

###list wesanderson movie pallette options###
#names(wes_palettes)

####print lat lon limits###
#Scotland.limits
