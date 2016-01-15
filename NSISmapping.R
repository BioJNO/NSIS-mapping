#### Load libraries ####
library(maps)
library(mapdata)
library(rworldmap)
library(ggmap)
library(wesanderson)

#### Get blank worldmap ####
newmap <- getMap(resolution = "high")

#### Set x and y limits to Scotland limits ####
Scotland.limits <- geocode(c("Out Stack, Shetland",
	"Cairngaan, Wigtownshire",
	"Rockall, Scotland",
	"Bound SKerry, Out Skerries, Shetland Islands")
)

#### List wesanderson movie pallette options ####
names(wes_palettes)

#### Set colour palletes ####
pal <- wes_palette("Zissou", 4, type = "continuous")
pal2 <- wes_palette("Darjeeling", type="continuous")

#### Print lat lon limits ####
Scotland.limits

##### Add function which adds alpha value (opacity) to colour. From Markus Gesmann (mages) on github <https://gist.github.com/mages/5339689#file-add-alpha-r> #####
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
#### Add alpha value to pal (background/ocean color) #####
palalpha <- add.alpha(pal, alpha=0.7)

#### Load in lat lon coordinates from file ####
#NSIS <- read.csv("waypoints.csv")
NSIS_recovered <- read.csv("NSIS_recovered_coordinates.csv")

#### Plot blank worldmap specifically Scotland #####
plot(newmap, 
	xlim = range(Scotland.limits$lon),
	ylim = range(Scotland.limits$lat),
	asp = 2,
	col=terrain.colors(30),
	bg=palalpha[1],
)

#### Plot lat lon coordinates from file ####
points(NSIS_recovered$LAT,NSIS_recovered$LON,col=pal2[1:4],cex=1.5,pch=17)
