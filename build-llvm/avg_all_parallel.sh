#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
machine=$2
makepath=$3
tarpath=$4
count=$5

MAX=1 # number of cores we have access to on hive

path=$(pwd)

tstamp=$(date +%s)
outfile="${path}/../../llvm-results/${tstamp}_${version}_${machine}.csv"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p ${path}/../../llvm-results

installprefix="${path}/../prefix"

mkdir -p ${installprefix}

touch $outfile
# write first line to file
echo "core, work (nanoseconds), avg (nanoseconds)" >> $outfile

mkdir -p ${makepath}

cd ${makepath}/..

echo "Cleaning"

rm -rf ${makepath}
rm -rf ${installprefix}
rm -rf ${execprefix}

printsfile="${path}/tmp.out"
 
sum=0
j=1

export PATH=/data/home.local/sjspall/projects/cmake-3.12.0-rc1-Linux-x86_64/bin:$PATH

while [ "$j" -le "$MAX" ] # need the spaces
do

    sum=0
    iters=0

    while [ "$iters" -lt "$count" ]
    do

	echo "un-taring"
	tar -xf ${tarpath}
	
	cd ${makepath}/..
	mkdir build && cd build
	
	echo "running configure and make"

	cmake ${makepath}

	mkdir -p /data/home.local/sjspall/tmp
	
	ov1=$(date +%s%N) && overhead=$((($(date +%s%N) - ${ov1})))
	
	tsl=$(date +%s%N) && make -j $j &>> ${printsfile} && tt=$((($(date +%s%N) - ${tsl} - ${overhead})))
	
	echo "make done"
	
	# write result to file
	echo "$j, $tt" >> $outfile
	
	# cleaning
	echo "Cleaning"
	
	cd ${makepath}/..
	
	rm -rf ${makepath}
	rm -rf build
	rm -rf /data/home.local/sjspall/tmp
	
	iters=$((iters + 1))
	sum=$((sum + tt))

	done

    avg=$((sum / count))
    echo "$j, , ${avg}" >> $outfile
    j=$((j + 1))

done

rm -f ${path}/tmp.out

echo "Done"

    

