### Read  Municipality data and create functions for obtaining the municipality of each eBird observation ###

# load packages and area shapefile into environment
library(rgdal)
library(sp)
my_spdf=readOGR("limites_municipalites", "limite_muni") 

# spdf (spatial polygon data frame) object --> plot
par(mar=c(0,0,0,0))
plot(my_spdf, col="#b2df8a", bg="#1f78b4", lwd=1, mar=rep(0,4), axes=TRUE)
axis(1);
# plot one polygon as an example
plot(my_spdf@polygons[[36]]@Polygons[[1]]@coords, col="#b2df8a", bg="#1f78b4", lwd=1, mar=rep(0,4))

# load up sp package for "point.in.polygon" function
transformed_spdf = sp::spTransform(my_spdf, CRS("+proj=longlat"))

# function to obtain the municipality from an observation's coordinates
getMunicipalite = function(dataset) {
  localisation = c()
  for (observation in c(1:nrow(dataset))) {
    for (polygon in c(1:length(transformed_spdf))) {
      for (sub_polygon in c(1:length(transformed_spdf@polygons[[polygon]]))) {
        pol = transformed_spdf@polygons[[polygon]]@Polygons[[sub_polygon]]@coords
        is_in_pol = point.in.polygon(dataset[observation,18], dataset[observation,17], pol[,1], pol[,2])
          #0: strictly exterior to pol, 1: strictly interior, 2: on the relative interior of an edge, 3: point is a vertex
        if (is_in_pol == 1) {
          localisation[observation] = as.character(muni$NM_MUN[polygon])
        }
      }
    }
  }
  return(localisation)
}

# function to find the area of each municipality
getArea = function(polygon_dataset) {
  for (observation in c(1:nrow(municipality))) {
    for (polygon in c(1:length(transformed_spdf))) {
      for (sub_polygon in c(1:length(transformed_spdf@polygons[[polygon]]))) {
        pol = transformed_spdf@polygons[[polygon]]@Polygons[[sub_polygon]]@coords
        area = 3
      }
    }
  }
  return(area)
}
