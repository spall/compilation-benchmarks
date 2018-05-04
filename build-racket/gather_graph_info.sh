#!/bin/bash

# args
version=$1
machine=$2
makepath=$3
tarpath=$4

# 1. add custom make script to front of path

oldpath=$PATH # save old path so we can restore when we are done
export PATH="/data/beehive/home.local/sjspall/compilation-benchmarks:$PATH" 
# 2. set MAKEJ = 1
export MAKEJ="1"
# 3. set output file
path=$(pwd)
tstamp=$(date +%s)
outfile="${path}/../../ocaml-results/rusage-out/${tstamp}_${version}_${machine}.debug" #maybe change extension
export OUTPUTFILE="${outfile}"

# create results directories if they don't exist
mkdir -p ${path}/../../racket-results
mkdir -p ${path}/../../racket-results/rusage-out

# clean things up so we are building fresh code.

mkdir -p ${makepath}
cd ${makepath}/..

echo "Cleaning"
rm -rf ${makepath}

echo "un-taring"

tar -xzf ${tarpath}

# broken racket specific thing
echo "Replacing broken rktio_process.c"

cp rktio_process.c ${makepath}/src/rktio/

cd ${makepath}/src
mkdir build
cd build

echo "building"

../configure &>> ${OUTPUTFILE} && make PLT_SETUP_OPTIONS="-j 1" && make PLT_SETUP_OPTIONS="-j 1" install

export PATH="$oldpath" # restore old path

echo "finished building"
echo "Debugging output written to ${OUTPUTFILE}"


