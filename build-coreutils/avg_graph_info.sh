#!/bin/bash

# args
version=$1
machine=$2
count=$3
makepath=$4
tarpath=$5

MAX=3 # number of cores we have access to on hive

# 3. set output file
path=$(pwd)
tstamp=$(date +%s)

outdir="${path}/../../coreutils-results/rusage-out/${tstamp}"
# create results directories if they don't exist
mkdir -p ${path}/../../coreutils-results
mkdir -p ${path}/../../coreutils-results/rusage-out
mkdir -p ${path}/../../coreutils-results/rusage-out/${tstamp}

# coreutils specific                                                                              
# need a installation prefix if we aren't super user                                                
installprefix="${path}/../prefix"
mkdir -p ${installprefix}
execprefix="${path}/../eprefix"
mkdir -p ${execprefix}

# clean things up so we are building fresh code.

mkdir -p ${makepath}
cd ${makepath}/..

echo "Cleaning"
rm -rf ${makepath}

# save old path so we can restore when we are done
oldpath=$PATH 

# 2. set MAKEJ = 1
export MAKEJ="1"

# 4. set up counter for shell commands
export SCNUM="/data/beehive/home.local/sjspall/compilation-benchmarks/scnum"

core=0

while [ "$core" -lt "$MAX" ] # need the spaces
do
    
    iters=0
    
    while [ "$iters" -lt "$count" ]
    do
	# set output file
	outfile="${outdir}/${core}_${iters}_${version}_${machine}.debug" #maybe change extension
	export OUTPUTFILE="${outfile}"

	echo "un-taring"

	tar -xf ${tarpath}
	cd ${makepath}
	
	echo "running configure"
	
	./configure --prefix=${installprefix} --exec-prefix=${execprefix} &>> ${OUTPUTFILE}

	echo "running make"

	export PATH="/data/beehive/home.local/sjspall/compilation-benchmarks:$PATH" 
	# 4. set up counter for shell commands
	echo 1 > $SCNUM # write 1 to file
    
	echo "building"
	
	taskset -ac $core make && taskset -ac $core make install
    
	export PATH="$oldpath" # restore old path
    
	echo "finished building"
	echo "Debugging output written to ${OUTPUTFILE}"

	echo "cleaning"

	cd ${makepath}/..
	rm -rf ${makepath}
	rm -rf ${installprefix}
	rm -rf ${execprefix}

	iters=$((iters + 1))
    done
    
    core=$((core + 1))
done

echo "Done"
