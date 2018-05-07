library(corrplot)


all_data <- read.csv("./full_dataframes/result_k_20_climate_soil.csv")[,-1]
just.parametrs <- all_data[,-c(1:23)]
ncol(just.parametrs)
M <- cor(just.parametrs)
corrplot(M, order = "AOE", cl.pos = "n", tl.pos = "n")