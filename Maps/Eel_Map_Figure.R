## Make a site map for the PAM 2014 and 2015 Experiments

## Angelo lat, long
## 39.718345, 123.652678


#### Libraries #################################################################
library(tidyverse)
library(ggmap)
library(rgdal)
library(broom)
library(ggsn)
#source("/Users/KeithBG/R_functions/ggplot_scalebar_north_arrow.R")
################################################################################

#### FILE PATHS ################################################################
gis_input <- file.path("/Users","KeithBG","Dropbox","PAM_Angelo","PAM_Angelo_Analyses", "Maps", "GIS_files")
dir_output <- file.path("/Users","KeithBG","Dropbox","PAM_Angelo","PAM_Angelo_Analyses", "Maps")
################################################################################


## Eel River networks
river.network.shp.import <- function(shp_pathway, layer_name, drainage_column= NA, min_drainage_area= 0){
  river.network.shp <- readOGR(dsn= shp_pathway, layer= layer_name)
  # Get column names in the attributes table
  print(ogrInfo(dsn= shp_pathway, layer= layer_name))

  # class(russian.network.shp) # SpatialLinesDataFrame
  # proj4string(russian.network.shp) # projection of shapefile
  if(min_drainage_area > 0){
    print(paste("Removing drainage areas <", min_drainage_area))
    col_num <- which(names(river.network.shp@data) %in% drainage_column) # get column number of drainage area data
    river.network.shp <- river.network.shp[river.network.shp@data[, col_num] >= min_drainage_area, ] # keep drainage areas >10 km^2
  }
  river.network.shp <- spTransform(river.network.shp, CRS("+proj=longlat +datum=WGS84")) # reproject to match ggmap
  river.network.shp <- tidy(river.network.shp)
  return(river.network.shp)
}

eel.network <- river.network.shp.import(shp_pathway = file.path(gis_input, "Eel_river_network_shapefile"),
                                        layer_name = "eel_rivernetwork_utm",
                                        drainage_column = "CUMDRAINAG",
                                        min_drainage_area = 100)

##  Watershed outlines
eel.watershed <- readOGR(dsn= file.path(gis_input, "Eel_River_Drainage_Area.kml"), layer= "Eel_River_Drainage_Area") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84")) %>%  # reproject to match ggmap
  tidy(.)

## California Outline
CA.outline <- readOGR(dsn= file.path(gis_input, "CA_boundary_shapefile"), layer= "CA_boundary") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84")) %>%  # reproject to match ggmap
  tidy(.)

## Make base_map
pam_map_theme <- theme(text= element_text(size= 14),
                          panel.background = element_rect(fill= "light blue"),
                          panel.border = element_rect(color= "black", fill= NA),
                          legend.key= element_rect(fill= "white"),
                          plot.background = element_rect(fill= "transparent", color= NA),
                          panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank()
)

## Make Map
base_map <- ggmap(get_stamenmap(bbox = c(left= -124.5, bottom= 37.3, right= -119.9, top= 42.1),
          zoom = 8,
          maptype = c( "terrain-background")))

  base_map +
  geom_path(data= CA.outline, aes(x= long, y= lat, group=group), color= "gray50") +
  geom_path(data= eel.network, aes(x= long, y= lat, group=group), color= "deepskyblue1", size= 0.75) +
  geom_polygon(data= eel.watershed, aes(x= long, y= lat, group=group), color= "Black", fill= NA) +
  scalebar(dist=25, location= "bottomleft", st.size = 3,  dd2km= TRUE, model = "WGS84", x.max= -124.2, y.min= 37.5, x.min= -119.9, y.max= 42.1)

# ggsave(last_plot(), filename= "eel_watershed_map.pdf", height= 6, width = 4, units= "in", path= dir_output)



# pam_site_map <- ggplot() +
#   geom_polygon(data= CA.outline, aes(x= long, y= lat, group=group), color= "black", fill= "snow1") +
#   geom_polygon(data= eel.watershed, aes(x= long, y= lat, group=group), color= "tomato", fill= NA) +
#   geom_path(data= eel.network, aes(x= long, y= lat, group=group), color= "#2A788EFF") +
#   scale_bar(lon = -124.4, lat = 38.3,
#             distance_lon = 20, distance_lat = 3, distance_legend = 6, dist_unit = "km",
#             arrow_length= 10, arrow_distance = 8) +
#   coord_map(xlim= c(-124.5, -120), ylim= c(37, 42.1)) +
#   #scale_y_continuous(breaks= c(38.5, 39, 39.5, 40, 40.5), labels= c("", "39", "", "40", "")) +
#   #scale_x_continuous(breaks= c(-124.5, -124, -123.5, -123.0, -122.5), labels= c("", "-124", "", "-123", "")) +
#   labs(x= "Longitude", y= "Latitude") +
#   pam_map_theme
# pam_site_map



