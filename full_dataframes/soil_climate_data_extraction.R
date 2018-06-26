library(psych)
library(raster)
library(sp)
library(rgdal)
library(RSQLite)


# biogeography is working directory
r <- raster::getData("worldclim",var="bio",res=5, path="./1_clime_data")
# coordinate.csv - file with longitude and latitude for samples.
# (http://1001genomes.org/accessions.html)
# mybed_450.20.Q - Q table from admixture
file <- read.csv("./full_dataframes/coordinate.csv")
tbl <- read.table("./full_dataframes/mybed_450.14.Q")


all_data <- cbind(file[c("tg_ecotypeid", "latitude", "longitude")], tbl)
# remove NA values from data
all_data <- na.omit(all_data)
write.csv(all_data[, -1], file = "./full_dataframes/lat_lon_Q_k_14.csv")

coords <- data.frame(x=all_data["longitude"],y=all_data["latitude"])
points <- SpatialPoints(coords, proj4string = r@crs)

# extract bio parameters from worldclim for selcted pointes 
values <- extract(r, points)
result_table <- cbind.data.frame(all_data["tg_ecotypeid"], coordinates(points),values)

# Q-table without NA coordinates
new_Q_table <- all_data[, -c(1:3)]


# rename bio parameters
colnames(result_table)[4:22] <- c("mean_annaul_temp" , "mean_diurnal_range",
                                  "isothermality", "sesonal_temp",
                                  "max_temp", "min_temp",
                                  "annual_temp_range", "mean_temp_wetq",
                                  "mean_temp_dryq", "mean_temp_warmq",
                                  "mean_temp_coldq", "annual_prec",
                                  "max_prec", "min_prec",
                                  "season_prec", "prec_wetq",
                                  "prec_dryq" , "prec_warmq",
                                  "prec_coldq")

# for other parameters from worldclim dowload manualy 5 minute 
# tar (remove readme.txt from folder)
# sun radiation
list <- list.files(path='./1_clime_data/wc2.0_5m_srad/', full.names=TRUE)
turaStack <- stack(list)
image(turaStack)
srad <- extract(turaStack, points)
result_table["solar_min"] <- apply(srad, 1, FUN=min)
result_table["solar_max"] <- apply(srad, 1, FUN=max) 
result_table["solar_mean"] <- rowMeans(srad) 

# wind speed
list <- list.files(path='./1_clime_data/wc2.0_5m_wind/', full.names=TRUE)
turaStack <- stack(list)
image(turaStack)
wind <- extract(turaStack, points)
result_table["wind_min"] <- apply(wind, 1, FUN=min)
result_table["wind_max"] <- apply(wind, 1, FUN=max) 
result_table["wind_mean"]<-rowMeans(wind) 

# water vapor pressure

list <- list.files(path='./1_clime_data/wc2.0_5m_vapr/', full.names=TRUE)
turaStack <- stack(list)
image(turaStack)
vapr <- extract(turaStack, points)
result_table["vapr_min"] <- apply(vapr, 1, FUN=min)
result_table["vapr_max"] <- apply(vapr, 1, FUN=max) 
result_table["vapr_mean"] <- rowMeans(vapr)

# soil parameters
hwsd.path <- './2_soil_data/hwsd.bil'
hwsd.sql.path <- './2_soil_data/HWSD.sqlite'

hwsd <- raster(hwsd.path)
MU_GLOBAL.in.points <- extract(hwsd, points)

m <- dbDriver("SQLite")
con <- dbConnect(m, dbname=hwsd.sql.path)

# According to Harmonized World Soil Database dockumentation 
# http://www.fao.org/fileadmin/templates/nr/documents/HWSD/HWSD_Documentation.pdf 
# numeric Physical and chemical characteristics of topsoil (0-30 cm) and subsoil (30-100 cm) is 
# T_GRAVEL
# T_SAND
# T_SILT
# T_CLAY 
# T_REF_BULK_DENSITY
# Density
# T_OC
# T_PH_H2O
# T_CEC_CLAY
# T_CEC_SOIL 
# T_BS 
# T_TEB 
# T_CACO3 
# T_CASO4 
# T_ESP
# T_ECE 
# S_GRAVEL
# S_SAND 
# S_SILT
# S_CLAY
# S_REF_BULK_DENSITY
# S_OC
# S_PH_H2O 
# S_CEC_CLAY
# S_CEC_SOIL
# S_BS
# S_TEB
# S_CACO3
# S_CASO4
# S_ESP S
# S_ECE  

soil.data.variables <- c("T_GRAVEL", "T_SAND", "T_SILT", "T_CLAY", 
                        "T_REF_BULK_DENSITY", "T_OC", "T_PH_H2O", 
                        "T_CEC_CLAY", "T_CEC_SOIL", "T_BS", "T_TEB", 
                        "T_CACO3", "T_CASO4", "T_ESP", "T_ECE", 
                        "S_GRAVEL", "S_SAND", "S_SILT", "S_CLAY", 
                        "S_REF_BULK_DENSITY", "S_OC", "S_PH_H2O", 
                        "S_CEC_CLAY", "S_CEC_SOIL", "S_BS", "S_TEB", 
                        "S_CACO3", "S_CASO4", "S_ESP", "S_ECE")


query.body <- paste("select",
                     paste(soil.data.variables, collapse = ", "),
                     "from HWSD_DATA where MU_GLOBAL = ")

soil.data.list <- lapply(MU_GLOBAL.in.points, function(mu_glob) {
    dbGetQuery(con, paste0(query.body, toString(mu_glob)))
})

result.of.join <- as.data.frame(lapply(1:length(soil.data.variables), function(x){1}))
names(result.of.join) <- soil.data.variables

for(row in soil.data.list) {
    result.of.join <- rbind(result.of.join, as.data.frame(lapply(row, mean, na.rm = T)))
}
result.of.join <- result.of.join[-1,]

result_table <- na.omit(cbind(new_Q_table, result_table, result.of.join))

# Final result 
write.csv(result_table , file = "./full_dataframes/result_k_14_climate_soil.csv")
