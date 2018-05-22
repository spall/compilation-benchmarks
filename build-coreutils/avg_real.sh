#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
machine=$2
count=$3
makepath=$4
tarpath=$5

MAX=32 # number of cores we have access to on hive

path=$(pwd)

tstamp=$(date +%s)
outfile="${path}/../../coreutils-results/${tstamp}_${version}_${machine}.csv"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p ${path}/../../coreutils-results

installprefix="${path}/../prefix"
execprefix="${path}/../eprefix"

mkdir -p ${installprefix}
mkdir -p ${execprefix}

touch $outfile
# write first line to file
echo "core, work (nanoseconds)" >> $outfile

mkdir -p ${makepath}

cd ${makepath}/..

echo "Cleaning"

rm -rf ${makepath}
rm -rf ${installprefix}
rm -rf ${execprefix}

printsfile="${path}/tmp.out"
 
sum=0
core=0
total=0

while [ "$core" -lt "$MAX" ] # need the spaces
do

    iters=0

    while [ "$iters" -lt "$count" ]
    do

	echo "un-taring"
	tar -xf ${tarpath}
	
	cd ${makepath}
	
	echo "running configure and make"
	
	./configure --prefix=${installprefix} --exec-prefix=${execprefix}
	
	ov1=$(date +%s%N) && overhead=$((($(date +%s%N) - ${ov1})))
	
	tsl=$(date +%s%N) && taskset -ac $core make -j 1 &>> ${printsfile} && taskset -ac $core make -j 1 install &>> ${printsfile} && tt=$((($(date +%s%N) - ${tsl} - ${overhead})))
	
	echo "make done"
	
	# write result to file
	echo "$core, $tt" >> $outfile
	
	# cleaning
	echo "Cleaning"
	
	cd ${makepath}/..
	
	rm -rf ${makepath}
	
	rm -rf ${installprefix}
	
	iters=$((iters + 1))
	sum=$((sum + tt))
	total=$((total + 1))

	done
    
    core=$((core + 1))

done

avg=$((sum / total))
echo "avg: ${avg}" >> $outfile

rm -f ${path}/tmp.out

echo "Done"

    

