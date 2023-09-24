#!/bin/bash
#SBATCH --time=20:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --mail-user=daniel.weinberger@yale.edu
#SBATCH --cpus-per-task=8
#SBATCH --array=1-108   # Specify the total number of tasks

# Define the mJ and K codes
module load R/4.2.0-foss-2020b

# Calculate the values of J and K for this task
START_VALUE_J=1
END_VALUE_J=108
START_VALUE_K=1
END_VALUE_K=10

# Calculate the task-specific values of J and K
task_id=$SLURM_ARRAY_TASK_ID
j=$(( (task_id - 1) % (END_VALUE_J - START_VALUE_J + 1) + START_VALUE_J ))
k=$(( (task_id - 1) / (END_VALUE_J - START_VALUE_J + 1) + START_VALUE_K ))

# Run your R script with the task-specific J and K
Rscript mod1.R "$j" "$k"