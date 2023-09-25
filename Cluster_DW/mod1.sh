#!/bin/bash
#SBATCH --time=20:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --mail-user=daniel.weinberger@yale.edu
#SBATCH --cpus-per-task=8
#SBATCH --array=1-1080   # If k models and J hold out time points this is 1- j*k

# Define the mJ and K codes
module load R/4.2.0-foss-2020b

# J:1-108 time periods
# K 1:10 models

# Use modulos to iterate through all task IDs
task_id=$SLURM_ARRAY_TASK_ID
j=$(( task_id  / 10 )) # $(( )) does arithmetic evaluation; Bash performs integer division so floor() is default
k=$(( task_id  % 10  + 1 )) # $(( )) does arithmetic evaluation

    # Run your R script with the task-specific J and K
Rscript mod1.R "$j" "$k"

# done
