# Download USA state boundaries from census website https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
# 20-June-2017

USA.state.boundaries <- readOGR(dsn= "/Users/kbg/Documents/UC_Berkeley/Manuscripts/CyanotoxinSurvey/GIS_data", layer= "cb_2016_us_state_500k")

ogrInfo(dsn= "/Users/kbg/Documents/UC_Berkeley/Manuscripts/CyanotoxinSurvey/GIS_data", layer= "cb_2016_us_state_500k")
# class(eel.network.shp) # SpatialLinesDataFrame
# proj4string(USA.state.boundaries) # projection of shapefile

# Subset to California
CA.boundary <- subset(USA.state.boundaries, NAME== "California")

# Write new shapefile
# writeOGR(CA.boundary, dsn= "/Users/kbg/Documents/UC_Berkeley/Manuscripts/CyanotoxinSurvey/GIS_data", layer= "CA_boundary_kbg2", driver= "ESRI Shapefile", morphToESRI = TRUE)
