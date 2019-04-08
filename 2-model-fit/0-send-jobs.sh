#!/bin/bash

maxsleep=5

echo "---------------------------------------"
echo "Please enter the models you wish to run (separated by spaces):"
read models
echo "---------------------------------------"

# when ran on the HPC, include this to make R findable
# echo "Loading R"
# source /opt/asn/etc/asn-bash-profiles-special/modules.sh
# module load R/3.3.3

# loop through models: create and execute model-specific programs
for model in ${models[@]}
do

# create a copy of 0a-job.sh with the third line changed to define the appropriate $model variable
awk 'NR==3 {$0="model="'$model'""} { print }' 0a-job.sh > 0b-job-$model.sh

# make the new file executable
chmod +x 0b-job-$model.sh

# execute the temporary verison with specific seed
sh 0b-job-$model.sh
# run_script 0b-job-$model.sh

# random sleep to avoid crashes
r=$(($RANDOM % $maxsleep))
echo "Random Sleep Time After Sending Model $model: $r seconds"
sleep $r

done

echo "All jobs sent to the HPC"
