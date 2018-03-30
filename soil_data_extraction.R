library(raster)


nutrient.availability <-
  as.matrix(raster("./quality/nutrient_availability.asc"))
nutrient.retention.capacity <-
  as.matrix(raster("./quality/nutrient_retention_capacity.asc"))
rooting.conditions <-
  as.matrix(raster("./quality/rooting_conditions.asc"))
oxygen.availability <-
  as.matrix(raster("./quality/oxygen_availability.asc"))
excess.salts <- as.matrix(raster("./quality/excess_salts.asc"))
toxicity <- as.matrix(raster("./quality/toxicity.asc"))
workability <- as.matrix(raster("./quality/workability.asc"))

get_soil_quality_by_coords <- function(lat, lon, matrix.data) {
  scale = dim(matrix.data) / c(180, 360)
  scaled.lat <- round((-lat + 90) * scale[1])
  scaled.lon <- round((-lon + 180) * scale[2])
  matrix.data[scaled.lat, scaled.lon]
}

bio_geo_data <- read.csv("./our_data/result_k_20_climate.csv")


bio_geo_data$nutrient.availability <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    nutrient.availability
  )
)

bio_geo_data$nutrient.retention.capacity <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    nutrient.retention.capacity
  )
)

bio_geo_data$rooting.conditions <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    rooting.conditions
  )
)


bio_geo_data$oxygen.availability <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    oxygen.availability
  )
)


bio_geo_data$excess.salts <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    excess.salts
  )
)

bio_geo_data$toxicity <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    toxicity
  )
)

bio_geo_data$workability <- with(
  bio_geo_data, 
  get_soil_quality_by_coords(
    latitude, 
    longitude, 
    workability
  )
)


write.csv(bio_geo_data,  file = "result_k_20_climate_soil.csv")