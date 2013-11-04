#!/bin/bash
if [ $# -eq 0 ] 
then
	echo "O0"
	for file in $(pwd)/*O0
	do
	  echo ${file##*/}
	  /$file
	done

	echo "O3"
	for file in $(pwd)/*O3
	do
	  echo ${file##*/}
	  /$file
	done
else
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
fi


