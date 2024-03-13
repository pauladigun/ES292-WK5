library(maptools)
library(raster)
library(ncdf4)
library(rgeos)


setwd("/Documents/china-m")
shp <- shapefile("/home/pauli/Documents/m/na/af.shp")

# Read the raster brick
data <- brick("tas.nc")


# Function to mask a raster layer with a shapefile
mask_layer <- function(layer, shape) {
  masked_layer <- mask(layer, shape)
  return(masked_layer)
}

masked_layers <- lapply(1:nlayers(data), function(i) mask_layer(data[[i]], shape = shp))

masked_brick <- brick(masked_layers)

# Write the masked raster brick to a new NetCDF file
writeRaster(masked_brick, "tas_out.nc", overwrite = TRUE, format = "CDF", varname = "tas",
            varunit = "k", longname = "air_temperature", xname = "lon", yname = "lat")



# export to csv

library(ncdf4)
nc = nc_open("tas_out.nc")
lat=ncvar_get (nc,"lat") ; lon=ncvar_get(nc,"lon")
lat ; lon
variable <- ncvar_get(nc)
# producing a timeseries
var_ts <- apply(variable,3,mean,na.rm=TRUE)
plot(var_ts, type = "l")
df1 = data.frame("tas" = var_ts )

write.csv(df1,"tas.csv", row.names = FALSE)
