#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
machine=$2
makepath=$3
tarpath=$4
count=$5

MAX=32 # number of cores we have access to on hive

path=$(pwd)

tstamp=$(date +%s)
outfile="${path}/../../coreutils-results/${tstamp}_${version}_${machine}.csv"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p ${path}/../../coreutils-results

installprefix="${path}/../prefix"
execprefix="${path}/../eprefix"

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
 
j=1

while [ "$j" -le "$MAX" ] # need the spaces
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
	
	tout=($(time (tsl=$(date +%s%N) && make -j $j &>> ${printsfile} && make -j $j install &>> ${printsfile} && tt=$((($(date +%s%N) - ${tsl} - ${overhead}))) && printf "%d, %d, " $j $tt >> $outfile) 2>&1))
	
	ut=${tout[3]} # user time
	st=${tout[5]} # sys  time

	echo "make done"
	
	# write result to file
	echo "$ut, $st" >> $outfile
	
	# cleaning
	echo "Cleaning"
	
	cd ${makepath}/..
	
	rm -rf ${makepath}
	
	rm -rf ${installprefix}
	rm -rf ${execprefix}

	iters=$((iters + 1))

	done

    j=$((j + 1))

done

rm -f ${path}/tmp.out

echo "Done"

    

