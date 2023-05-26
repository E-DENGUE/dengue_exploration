library(parallel) 
library(future)

source('base_model_cluster.R')

n_cores <- max(availableCores(methods=c("system")) - 1, 1)

cl1 <- parallel::makeCluster(n_cores)


clusterEvalQ(cl=cl1, {
  library(lubridate, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(CARBayes)
  library(CARBayesST)
  library(TSDT)
})

clusterExport(cl1, c('mod.func','advance_missing_func', 'base_model_final', 'neighb.mat_2'), environment())

mod1 <- parLapply(
  cl = cl1,
  74:145,
  fun = mod.func
)

stopCluster(cl1)