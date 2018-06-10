#!/bin/bash

# args
version=$1
machine=$2
count=$3
makepath=$4
tarpath=$5

MAX=1 # number of cores we have access to on hive

# 1. add custom make script to front of path
path=$(pwd)

oldpath=$PATH # save old path so we can restore when we are done
export PATH="~/bin:$path/../custom-make:$PATH" 
# 2. set MAKEJ = 1
export MAKEJ="1"


tstamp=$(date +%s)

outdir="${path}/../../ocaml-results/rusage-out/${tstamp}"

# 4. set up counter for shell commands
export SCNUM="$path/../scnum"
echo 1 > $SCNUM # write 1 to file

# create results directories if they don't exist
mkdir -p ${path}/../../ocaml-results/
mkdir -p ${path}/../../ocaml-results/rusage-out
mkdir -p ${outdir}

# ocaml specific
# need a installation prefix if we aren't super user
installprefix="${path}/../prefix"

mkdir -p ${makepath}
cd ${makepath}/..

echo "Cleaning"
rm -rf ${makepath}
rm -rf ${installprefix}

core=0

while [ "$core" -lt "$MAX" ]
do
    iters=0
    
    while [ "$iters" -lt "$count" ]
    do
	# 3. set output file
	outfile="${outdir}/${core}_${iters}_${version}_${machine}.debug" #maybe change extension
	export OUTPUTFILE="${outfile}"
	
	echo "un-taring"

	tar -xzf ${tarpath}
	cd ${makepath}
	
	# another ocaml specific thing
	env TMPDIR="${path}/tmp"
	
	echo "building"
	
	./configure --prefix ${installprefix}

	make world && make install

	echo "make done"

	echo "cleaning"
	
	cd ${makepath}/..
	rm -rf ${makepath}
	rm -rf ${path}/tmp
	rm -rf ${installprefix}

	iters=$((iters + 1))
    done

    core=$((core + 1))
done

export PATH="$oldpath" # restore old path
