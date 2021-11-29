#!/bin/sh

mkdir -p temp

for file in bench/data/*; do
  echo $file
  ./baker $file temp/temp.txt
  python check.py $file temp/temp.txt
done