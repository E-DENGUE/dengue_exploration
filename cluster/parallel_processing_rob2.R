library(parallel) 

source('base_model_cluster.R')

n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK", 1)

cl1 <- parallel::makeForkCluster(n_cores)

mod1 <- parLapply(
  cl = cl1,
  1:73,
  fun = mod.func
)

stopCluster(cl1)

