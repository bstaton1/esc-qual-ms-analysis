#!/bin/bash

# read the model number from user
echo -ne "Model: "
read model

# read the node number from user
echo -ne "Hopper Node: "
read node

# create the job file
Rscript create-job.R $model $node

# send the job to the queue
qsub 0-job-model-$model.sh
