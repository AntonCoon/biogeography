# tbl <- read.table("./our_data/mybed_450.20.Q")
# barplot(t(as.matrix(tbl)), col=rainbow(12),
#         xlab="Individual #", ylab="Ancestry", border=NA)
# barplot(t(as.matrix(tbl))[2,],
#         xlab="Individual #", ylab="Ancestry", border=NA)
# t(as.matrix(tbl))[1,]
# 
# View(read.csv("./our_data/result_k_20_climate_soil.csv"))


bio_geo_data <- read.csv("./our_data/result_k_20_climate_soil.csv")

names(bio_geo_data)
