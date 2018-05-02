library(psych)
library(raster)
library(sp)
library(rgdal)
# library(rhwsd)

setwd(file.path(getwd(), "full_dataframes"))
r <- raster::getData("worldclim",var="bio",res=5, path="../1_clime_data")


# coordinate.csv - file with longitude and latitude for samples.
# (http://1001genomes.org/accessions.html)
# mybed_450.12.Q - Q table from admixture
file <- read.csv("coordinate.csv")
tbl <- read.table("mybed_450.12.Q")


all_data <- cbind(
  file[c("tg_ecotypeid", "latitude", "longitude")], tbl)
# remove NA values from data
all_data <- na.omit(all_data)


coords <- data.frame(x=all_data["longitude"],y=all_data["latitude"])
points <- SpatialPoints(coords, proj4string = r@crs)

# extract bio parameters from worldclim for selcted pointes 
values <- extract(r, points)
result_table <- cbind.data.frame(all_data["tg_ecotypeid"], coordinates(points),values)

## plot pointes in map
# plot(r[[1]])
# plot(points,add=T)

# Q-table without NA coordinates
new_Q_table <- all_data[, 4:15]
write.csv(new_Q_table , file = "new_Q_table_12.csv")

# rename bio parameters
# colnames(result_table)[4:22] <- c("mean_annaul_temp" , "mean_diurnal_range", 
#                                   "isothermality", "sesonal_temp", 
#                                   "max_temp", "min_temp", 
#                                   "annual_temp_range", "mean_temp_wetq", 
#                                   "mean_temp_dryq", "mean_temp_warmq", 
#                                   "mean_temp_coldq", "annual_prec",
#                                   "max_prec", "min_prec",
#                                   "season_prec", "prec_wetq",
#                                   "prec_dryq" , "prec_warmq",
#                                   "prec_coldq")
# colnames(result_table)

# for other parameters from worldclim dowload manualy 5 minute 
# tar (remove readme.txt from folder)
# sun radiation
list <- list.files(path='../1_clime_data/wc2.0_5m_srad/', full.names=TRUE)
turaStack <- stack(list)
image(turaStack)
srad <- extract(turaStack, points)
result_table["solar_min"] <- apply(srad, 1, FUN=min)
result_table["solar_max"] <- apply(srad, 1, FUN=max) 
result_table["solar_mean"] <- rowMeans(srad) 

# wind speed
list <- list.files(path='../1_clime_data/wc2.0_5m_wind/', full.names=TRUE)
turaStack <- stack(list)
image(turaStack)
wind <- extract(turaStack, points)
result_table["wind_min"] <- apply(wind, 1, FUN=min)
result_table["wind_max"] <- apply(wind, 1, FUN=max) 
result_table["wind_mean"]<-rowMeans(wind) 

# water vapor pressure
list <- list.files(path='../1_clime_data/wc2.0_5m_vapr/', full.names=TRUE)
turaStack <- stack(list)
image(turaStack)
vapr <- extract(turaStack, points)
result_table["vapr_min"] <- apply(vapr, 1, FUN=min)
result_table["vapr_max"] <- apply(vapr, 1, FUN=max) 
result_table["vapr_mean"] <- rowMeans(vapr)

# soil parameters
# list <- list.files(path='../1_clime_data/soil/', full.names=TRUE)
# turaStack <- stack(list)
# image(turaStack)
# vapr <- extract(turaStack, points)
# 
# result_table <- cbind.data.frame(result_table,vapr)

# result table
write.csv(result_table , file = "result_k_12_climate_soil.csv")









hwsd.path <- '../2_soil_data/HWSD_RASTER/hwsd.bil'
hwsd.sql.path <- '../2_soil_data/HWSD.sqlite'

hwsd <- raster(hwsd.path)
plot(hwsd)
(proj4string(hwsd) <-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

hwsd.in.points <- extract(hwsd, points)
str(hwsd.in.points)

m <- dbDriver("SQLite")
con <- dbConnect(m, dbname=hwsd.sql.path)
dbListTables(con)
dbGetQuery(con, "pragma table_info(HWSD_DATA)")$name
dbGetQuery(con, "pragma table_info(HWSD_DATA)")$type



