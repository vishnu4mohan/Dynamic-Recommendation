rm(list = ls(all = TRUE))
options(scipen = 999)
options(java.parameters = "-Xmx15g")
pkgLoad <- function(package) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, dep=TRUE, repos = "http://cran.csie.ntu.edu.tw/")
        if(!require(package, character.only = TRUE)) stop("Package not found")
    }
    suppressMessages(library(package, character.only=TRUE))
}
pkgLoad("data.table")
pkgLoad("h2o")
pkgLoad("properties")
props<-read.properties("reco.property")
setwd(props$CLUSTER_PATH)
num_clusters <- props$NUM_CLUSTERS
inputFilePath <- "/opt/RE/RECO/RData/OutlierTreatFile.csv"
if(!"h2o" %in% installed.packages()){
	stop("Please install h2o package to continue.")
} else {
		get_free_ram <- function(){
		system <- Sys.info()[["sysname"]]
		if (system == "Linux"){
			x <- system('grep MemFree /proc/meminfo', intern = TRUE)
			x <- gsub("MemFree:        ", "",x, fixed = TRUE)
			x <- gsub(" kB", "", x, fixed = TRUE)
			as.integer(x)
		}  else {
				stop("Only supported on Linux OS")
			}
		}
		#try(h2o.init(nthreads=-1, max_mem_size = paste(ceiling(get_free_ram()/1024^2),"g", sep = ""), enable_assertions = FALSE), silent = TRUE)
		try(h2o.init(nthreads=-1, min_mem_size = '17g', max_mem_size = '17g', enable_assertions = FALSE), silent = TRUE)
		if(h2o.clusterIsUp()){
			cluster_status <- h2o.clusterStatus()
			print(paste("H2O Cluster Total Memory", as.numeric(cluster_status$free_mem), sep = "-"))
			print(paste("Number of CPUs in Use", as.numeric(cluster_status$num_cpus), sep = "-"))
			getColumnsType <- function(data) {
				if(is.h2o(data)){
					header <- h2o.names(data)
					numeric_list <- NULL
					non_numeric_list <- NULL
					others_list <- NULL
					for(col in header) {
						ifelse(h2o.isax(data[, col], num_words = 10, max_cardinality = 10), others_list <- c(others_list, col),
								ifelse(h2o.isfactor(data[, col]) | h2o.ischaracter(data[, col]), non_numeric_list <- c(non_numeric_list, col),
										ifelse(h2o.isnumeric(data[, col]), numeric_list <- c(numeric_list, col), . <- c(non_numeric_list, col))
								)
						)
					}
					return(list(numeric_list, non_numeric_list, others_list))
				}else {
					print("Not an H2O Data Frame..!!!")
					return(NULL)
				}
			}
			clustering <- function(data){
				cluster_list <- NULL
				kmeans_model <- NULL
				col_types <- getColumnsType(data)
				numeric_list <- unlist(col_types[1])
				non_numeric_list <- c("R_MSISDN",c(unlist(col_types[2])))
				if(!is.data.frame(data)){
					data <- as.data.frame(data)
				}
				scaleData <- scale(data[, -which(names(data) %in% non_numeric_list)])
				scaleData[is.na(scaleData)] <- 0
				fit <- kmeans(scaleData, num_clusters)
				data <- data.frame(data, fit$cluster)
				clusters <- unique(fit$cluster)
				for(i in clusters){
					cluster<-filter(data, data$fit.cluster == i)
					fwrite(cluster, file = paste("Cluster_",i, ".csv",sep=""),row.names=FALSE)
				}
			}
			data <- h2o.importFile(inputFilePath)
			clustering(data)
		} else {
			stop("Couldn't Connect to H2O Cluster.")
		}
	h2o.removeAll()
	try(h2o.shutdown(prompt=TRUE), silent = TRUE)
	detach("package:data.table",unload=TRUE)
	detach("package:h2o",unload=TRUE)
	rm(list = ls(all = TRUE))
}