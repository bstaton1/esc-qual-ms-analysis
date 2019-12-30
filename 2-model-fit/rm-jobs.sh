#!/bin/bash

files=$(ls 0-job-model*)

echo "Model-specific job scripts found:"
echo $files
sleep 0.5
echo ""
echo "Are you sure you wish to remove these files? (y/n)"
read remove
echo ""

if [ $remove == "y" ]
then
  rm 0-job-model*
  echo "  Okay, these files were removed (you're welcome)."
else
  echo "  Okay, these files will be kept (you're welcome for asking)."
fi
