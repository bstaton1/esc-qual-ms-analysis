#!/bin/bash

# model goes here. will get placed here with awk when this file is copied

# sleep for a random amount of time
# this is in case two jobs start at the exact same time
# it will cause it to crash with a module loading error
r=$(($RANDOM % 5))
echo "Random Sleep Time Before Starting Model $model: $r seconds"
sleep $r

# when ran on the HPC, include this to make R findable
# echo "Loading R"
# source /opt/asn/etc/asn-bash-profiles-special/modules.sh
# module load R/3.3.3

# get the date/time of the start of this script
d=$(date)

echo "+++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "Running Model $model"
echo "  Started running at: $d"
Rscript 2-fit-model-any.R $model 

# print the end date and time of this script
d=$(date)
echo "  Ended running at:   $d"
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++"
