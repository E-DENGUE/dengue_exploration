#!/bin/bash

#SBATCH --time=20:00:00
#SBATCH --mail-type=NONE -p scavenge
#SBATCH --cpus-per-task=3
##SBATCH --mem=181G


module load R/4.0.5-foss-2020b
Rscript parallel_processing_rob2.R
