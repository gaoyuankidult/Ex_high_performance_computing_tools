#!/bin/bash

echo "O0"
for file in $(pwd)/$1_*O0
do
  echo ${file##*/}
  /$file
done

echo "O3"
for file in $(pwd)/$1_*O3
do
  echo ${file##*/}
  /$file
done



