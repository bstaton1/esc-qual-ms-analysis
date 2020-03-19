#!/bin/bash

mkdir esc-qual-bundle
cd model-output

echo "Models with completed output:"
ls post* | grep -o [0-9]*

echo "---------------------------------------"
echo "Which models do you want to bundle?"
echo "  (separated by spaces):"
read models
echo "---------------------------------------"
echo "Name of tar.gz file?"
read filename

echo "Making Copies"
for model in ${models[@]}
do
  echo "  Model $model Output"
  cp *-$model.rds ../esc-qual-bundle
done

cd ../

echo "Creating Tarball:"
tar -cvzf $filename esc-qual-bundle

rm -r esc-qual-bundle
