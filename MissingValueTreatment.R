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
inputFilePath <- "{RatingFile}"
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
		} else {
				stop("Only supported on Linux OS")
			}
		}
		#try(h2o.init(nthreads=-1, max_mem_size = paste(ceiling(get_free_ram()/1024^2),"g", sep = "")), silent = TRUE)
		try(h2o.init(nthreads=-1, min_mem_size = '17g', max_mem_size = '17g', enable_assertions = FALSE), silent = TRUE)
		if(h2o.clusterIsUp()){
			cluster_status <- h2o.clusterStatus()
			print(paste("H2O Cluster Total Memory", as.numeric(cluster_status$free_mem), sep = "-"))
			print(paste("Number of CPUs in Use", as.numeric(cluster_status$num_cpus), sep = "-"))
			missing_val_treat <- function(data){
				header <- h2o.names(data)
				data<-as.h2o(data)
				for(col in header) {
					if(h2o.sum(is.na(data[, col])) > 0){
						if(h2o.isnumeric(data[, col])){
							na_count <- h2o.sum(is.na(data[, col]))
							mean_col <- h2o.asnumeric(h2o.mean(data[, col], na.rm = TRUE))
							sd_col <- h2o.asnumeric(h2o.sd(data[, col], na.rm = TRUE))
							upper_count <- mean_col + (3 * sd_col)
							lower_count <- mean_col - (3 * sd_col)
							total <- h2o.nrow(data[(data[, col] >= lower_count) & (data[, col] <= upper_count),])
							na_per <- (na_count/nrow(data))
							threshold <- (total/nrow(data))
							if(na_per > 0.65){
								data[, col][is.na(data[, col])] <- 0
							} else if(threshold >= 0.997){
								data[, col][is.na(data[, col])] <- mean_col
							} else {
								data[, col][is.na(data[, col])] <- 0
							}
						} else if(h2o.ischaracter(data[, col]))
						{
							data[, col][is.na(data[, col])] <- "Unknown"
						} else if(h2o.isfactor(data[, col])){
							data[ ,col] <- h2o.ascharacter(data[ ,col])
							data[, col][is.na(data[, col])] <- "Unknown"
							data[ ,col] <- h2o.asfactor(data[ ,col])
						}
					}
				}
				return(data) 
			}
			data <- h2o.importFile(inputFilePath)
			TreatFile <- missing_val_treat(data)
			h2o.exportFile(TreatFile, "/opt/RE/RECO/RData/TreatFile.csv", force = TRUE, parts = 1)
		} else {
			stop("Couldn't Connect to H2O Cluster.")
		}
	h2o.removeAll()
	try(h2o.shutdown(prompt=TRUE), silent = TRUE)
	detach("package:data.table",unload=TRUE)
	detach("package:h2o",unload=TRUE)
	rm(list = ls(all = TRUE))
}