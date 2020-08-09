#!/bin/bash

jobs=$(ls 0-job-model*)
progress=$(ls progress*)
errors=$(ls errors*)

echo "Model-specific job scripts found:"
echo $jobs
echo "Model-specific progress outputs found:"
echo $progress
echo "Model-specific error outputs found:"
echo $errors
sleep 0.5
echo ""
echo "Are you sure you wish to remove these files? (y/n)"
read remove
echo ""

if [ $remove == "y" ]
then
  rm 0-job-model*
  rm progress*
  rm errors*
  echo "  Okay, these files were removed (you're welcome)."
else
  echo "  Okay, these files will be kept (you're welcome for asking)."
fi
