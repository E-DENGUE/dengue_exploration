library(parallel) 

source('precipitation_model.R')

n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK", 1)

mod1 <- mclapply(0:144, mod.func, mc.cores=n_cores)