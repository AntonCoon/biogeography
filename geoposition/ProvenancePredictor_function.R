## R script that implents the ProvenancePredictor algorithm
## (c) L. GENTZBITTEL, C. Ben and T.V. TATARINOVA, 2015
library(Cairo)
library(rworldmap)
library(rworldxtra)
library(ade4)


lapply(1:25, function(index) {
    path.to.data <- file.path(getwd(), "full_dataframes", paste0("res_", toString(index), ".csv"))
    if (file.exists(path.to.data)) {
        log.file.name <- file.path(getwd(), 
                                   "geoposition",
                                   paste0("log_", toString(index), ".txt"))

        log.file <- file(log.file.name)

        N_best <- index - 1
        GEOALL <- read.csv(path.to.data, header = T, row.names = 1)
        q.var.names <- unlist(lapply(c(1:index), function(x) paste0("V", toString(x))))

        colnames(GEOALL) <- c(q.var.names, "Pop", "Lat", "Lon")

        GEO <- GEOALL[!is.na(GEOALL$Lat), c('Lon', 'Lat')]
        Inconnues <- GEOALL[is.na(GEOALL$Lat), ]

        ADMIXTURES0 <- as.matrix(GEOALL[, c(1:index)])

        nk <- dim(GEO)[1]
        ADMIXTURES <- ADMIXTURES0[1:nk, ]
        rownames(ADMIXTURES) <- rownames(GEOALL)

        Inconnues2 <- data.frame(
            ADMIXTURES[rownames(ADMIXTURES) %in% rownames(Inconnues), ], 
            GROUP = Inconnues$Pop)

        write.table(Inconnues2, paste0("./geoposition/output/inconnues", toString(index), ".csv"), sep = "\t")
        GEN <- ADMIXTURES[!is.na(GEOALL$Lat), ]

        y <- dist(GEO)
        R <- 6371 # Earth mean radius [km]
        ykm <- y * R * pi / 180
        x <- dist(GEN)
        
        (RoughCorrels <- mantel.rtest(x, y, nrepet = 30))

        LL <- length(y)
        seuilGeo <- 8.25
        seuilGen <- 1.2
        for (l in 1:LL) {
            if ( y[l] >= seuilGeo || x[l] >= seuilGen ) {y[l] = 0; x[l] = 0;}
        }

        (FilteredCorrels <- mantel.rtest(x, y, nrepet = 30))

        eq1 <- lm(y ~ x)
        writeLines("lm result", log.file)
        sink(log.file.name)
        print(summary(eq1))

        UNKNOWN_DATA <- read.csv(paste0("./geoposition/output/inconnues", toString(index), ".csv"), 
                                 header = TRUE, 
                                 row.names = 1, 
                                 sep = "\t")
        GROUPS <- unique(UNKNOWN_DATA$GROUP)
        outfile_name <- paste0("./geoposition/output/inconnuesPredites", toString(index), ".csv")
        write("Population\tSample_no\tSample_id\tCentroidLon\tCentroidLat\tPredLon\tPredLat\tidentical accessions\tclosest accessions", outfile_name,  append=FALSE)

        N_best <-min(N_best, length(GEO[, 1]))

        for(GROUP in GROUPS){
            Y <-  UNKNOWN_DATA[UNKNOWN_DATA$GROUP == GROUP, ]
            K <- length(Y[,1])
            for(a in 1: K) {
                X <- Y[a, 1:index]
                E <- rep(0, length(GEO[, 1]))
                for(g in 1: length(GEO[, 1])){
                    ethnic <- attributes(GEO[g,])$row.names
                    gene <- as.numeric(GEN[ethnic, 1:index])
                    E[g] <- sqrt(sum((gene - X)^2))
                }
                minEb <- NULL
                minEb <- (rank(E, ties.method = "min") <= (sum(rank(E, ties.method = "min") == 1) + N_best))
                minEident <- rank(E,ties.method = "min") == 1
                minEg <- E[minEb]
                sort(minEg)
                minGb <- which(minEb == TRUE)[order(minEg)]
                minGident <- which(minEident == TRUE)
                minGclose <- minGb[!minGb %in% minGident]
                if (length(minGident) > 0) {
                    centroid <- colMeans(GEO[minGident,c('Lon','Lat')],na.rm=TRUE)
                } else {
                    centroid <- colMeans(
                        GEO[minGclose[rank(minEg[minGclose], ties.method = "min") == 1], c('Lon','Lat')]
                    )
                }
                radius <- sort(minEg)
                best_ethnic <- attributes(GEO[minEb,])$row.names
                identical.ethnic <- attributes(GEO[minEident,])$row.names
                radius_geo <- (eq1[[1]][2] * radius[1])
                W <- ((minEg[1] + 1e-9) / (minEg + 1e-9)) ^ (1/4)
                W <- W / (sum(W))
                delta_lon <- GEO[minGb, ][[1]] - centroid[1]
                delta_lat<- GEO[minGb, ][[2]] - centroid[2]
                new_lon <- sum(W * delta_lon)
                new_lat <- sum(W * delta_lat)
                lo1 <- new_lon * min(1,radius_geo / sqrt(new_lon^2 + new_lat^2))
                la1 <- new_lat * min(1,radius_geo / sqrt(new_lon^2 + new_lat^2))
                best <- NULL
                best[1] <- centroid[1] + lo1
                best[2] <- centroid[2] + la1
                write(
                    paste(GROUP, a, row.names(Y[a,]), 
                          centroid[1], 
                          centroid[2], 
                          best[1], 
                          best[2], 
                          paste(identical.ethnic,sep='',collapse="/"), 
                          paste(best_ethnic,sep='',collapse="/"), sep="\t"), 
                    outfile_name,append=T)
            }
        }
        print("RoughCorrels")
        print(RoughCorrels)
        print("FilteredCorrels")
        print(FilteredCorrels)
        sink()
        print(paste0("ProvenancePredictor is done for k = ", toString(index)))
    }
})
