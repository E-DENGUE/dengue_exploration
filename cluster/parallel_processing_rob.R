library(parallel) 

source('base_model_cluster.R')

n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK", 1)

sink('mod1_2.R')

mod1 <- mclapply(1:2, mod.func, mc.cores=n_cores)

sink()