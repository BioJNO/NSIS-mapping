# Load required packages -----------------------------------------------------
library(ggplot2)
library(rgdal)
library(maptools)
library(ggmap)
library(dplyr)
library(reshape2)
library(gridExtra)
library(grid)

# Plot a base R map of Scotland ----------------------------------------------
# Read shapefile data.
scotland = readOGR(dsn="Scotland_caspcs_2001",
                   layer="scotland_caspcs_2001")
scotland@data$id = rownames(scotland@data)
scotland.points = fortify(scotland, region="id")
scotland.df = left_join(scotland.points, scotland@data, by="id")

# Plot the base map
basemap = ggplot(scotland.df, aes(long, lat, group=group))
basemap = basemap + geom_polygon(fill="gray75") +
          theme(panel.background = element_rect(fill = "gray95"))
basemap = basemap + coord_equal()
basemap

# Load in ZOTU data ----------------------------------------------------------
# Load both samples which did and did not amplify.
zotus <- read.csv("zero_nsis_master_merged_3.csv")
nonzerozotus <- read.csv("non_zero_nsis_master_merged_3.csv")
colnames(zotus)

# Filter the ZOTU data frame to leave only the most abundant 
# for ease of plotting.
#
# First make a list where TRUE means a column will be kept,
# and FALSE means a column will be discarded. 
#
# Set TRUE explicitly for values you want to keep (metadata),
# and set a condition for the ZOTU columns (>500).
drop <- c(rep(TRUE, 12), colSums(nonzerozotus[,13:47]) > 500, rep(TRUE, 93))
nonzerozotus <- nonzerozotus[,drop]
colnames(nonzerozotus)

# Change the structure of the data frame -------------------------------------
# Select only the ZOTU columns from the data frame.
mnonzero <- nonzerozotus[,c(13:25)]
# Transpose the ZOTUs.
mnonzero <- t(mnonzero)
# Take the long format ZOTU table and stack the columns into one.
dat.m <- melt(mnonzero, id.vars = "sample")
# Give the columns in the stacked dataframe sensible names.
dat.m <- dat.m %>% rename(colid=Var2, zotu=Var1, reads=value)
# Set nonzerozotus as rownames
nonzerozotus$colid <- rownames(nonzerozotus)
# Merge stacked dataframe with sample dataframe and metadata.
merged <- merge(dat.m, nonzerozotus, by = "colid")
# Filter the merged dataframe to exclude samples with no merged read pairs.
merged <- merged[merged$reads > 0, ]
colnames(merged)
# Convert total assembled reads to amplicons per microliter.
merged$amplicons_per_ul <- merged$reads/merged$vol


# Generate map plots ---------------------------------------------------------
# Extract the columns required for plotting: 
# zotuid, sample, easting, northing, and amplicons per ul.
zotuplotpoints <- merged[,c(2,5,39,40, 122)]

# Plot all samples included in the dataset after filtering.
p1 = basemap + geom_point(mapping = aes(EASTING, NORTHING),
                          color = "white",
                          data = zotus)
p1

# To the plot contatining all sample points (p1), add the zotus
# sized by the amplicons per microliter, setting points as 
# semi-transparent (alpha=0.3). and with no legend.
p2 = p1 + geom_point(mapping = aes(EASTING, NORTHING,
                                   color = zotu,
                                   fill = zotu,
                                   group = 1,
                                   size = amplicons_per_ul),
                                   alpha = 0.3,
                                   shape = 21,
                                   data=zotuplotpoints) +
                     theme(legend.position = "none")
p2

# Save as a scalable vector graphic.
ggsave("NSIS_ZOTU_map.svg", dpi=600, width=4, height =6)

# Generate seperate legend plots ---------------------------------------------
# Define a function to extract the legend from the ggplot object.
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# Generate the map plot again with a legend.
p2 = p1 + geom_point(mapping = aes(EASTING, NORTHING,
                     color = zotu,
                     fill = zotu,
                     group = 1,
                     size = amplicons_per_ul),
                     alpha = 0.3, shape = 21,
                     data = zotuplotpoints)
p2

# Extract the legend from the ggplot object and plot it.
legend <- g_legend(p2)
legend
# Store the legend in a variable.
g <- grid.arrange(arrangeGrob(legend))

# Save the legend as a scalable vector graphic.
ggsave("map_legend.svg", g, dpi = 600, width = 3, height = 7)
