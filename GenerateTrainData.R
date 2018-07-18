rm(list = ls(all = TRUE))
options(scipen = 9999)
pkgLoad <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dep = TRUE, repos = "http://cran.csie.ntu.edu.tw/")
    if (!require(package, character.only = TRUE))
      stop("Package not found")
  }
  suppressMessages(library(package, character.only = TRUE))
}
pkgLoad("data.table")
pkgLoad("h2o")
pkgLoad("dplyr")
pkgLoad("properties")
props <- read.properties("/opt/RE/RuleEngine6/bin/reco.property")
cluster_path <- props$CLUSTER_PATH
numClusters <- props$NUM_CLUSTERS
model_path <- props$MODEL_PATH
max_mem <- props$H2O_MAX_MEMORY
min_mem <- props$H2O_MIN_MEMORY
test_regx <- "^((M1_)|(M2_)|(M3_)|(M1M2M3_)).*$"
train_regx <- "^((M3_)|(M4_)|(M5_)|(M3M4M5_)).*$"
churners_file <- "{TRAIN_Y}"
if (!"h2o" %in% installed.packages()) {
  stop("Please install h2o package to continue.")
} else{
  try(h2o.init(
    nthreads = -1,
    min_mem_size = max_mem,
    max_mem_size = min_mem,
    enable_assertions = FALSE
  ),
  silent = TRUE)
  if (h2o.clusterIsUp())
  {
    cluster_status <- h2o.clusterStatus()
    print(paste(
      "H2O Cluster Total Memory",
      as.numeric(cluster_status$free_mem),
      sep = "-"
    ))
    print(paste(
      "Number of CPUs in Use",
      as.numeric(cluster_status$num_cpus),
      sep = "-"
    ))
    base_churners <-
      h2o.importFile(path = churners_file,
                     destination_frame = "base_churners",
                     sep = ",")
    for (i in 1:numClusters)
    {
      input_file <-
        h2o.importFile(paste(cluster_path, "Churn_Cluster_", i, ".csv", sep = ""), sep = ",")
      churners <-
        h2o.merge(input_file, base_churners, by = "R_MSISDN")
      non_churners <- anti_join(input_file, churners)
      churners$STATUS <- "Y"
      non_churners$STATUS <- "N"
      new_non_churners <-
        h2o.sample(non_churners, nobs = nrow(churners))
      churners <- churners[, c("R_MSISDN", grepl(train_regx, h2o.names(churners)), "STATUS")]
      new_non_churners <- new_non_churners[, c("R_MSISDN", grepl(train_regx, h2o.names(churners)), "STATUS")]
      train_data <- h2o.rbind(churners, new_non_churners)
      rename <- gsub("M3", "M1", h2o.names(train_data))
      rename <- gsub("M4", "M2", rename)
      rename <- gsub("M5", "M3", rename)
      names(train_data) <- rename
      h2o.exportFile(
        train_data,
        paste(cluster_path, "Train_Cluster_", i, ".csv", sep = ""),
        force = TRUE,
        parts = 1
      )
      test_data <- input_file[, c("R_MSISDN", grepl(test_regx, h2o.names(input_file)))]
      h2o.exportFile(
        test_data,
        paste(cluster_path, "Test_Cluster_", i, ".csv", sep = ""),
        force = TRUE,
        parts = 1
      )
    }
  } else {
    stop("Couldn't Connect to H2O Cluster.")
  }
  h2o.removeAll()
  try(h2o.shutdown(prompt = TRUE), silent = TRUE)
  detach("package:data.table", unload = TRUE)
  detach("package:h2o", unload = TRUE)
  detach("package:properties", unload = TRUE)
  detach("package:dplyr", unload = TRUE)
  rm(list = ls(all = TRUE))
}