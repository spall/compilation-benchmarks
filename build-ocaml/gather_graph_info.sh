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

# 4. set up counter for shell commands
export SCNUM="/data/beehive/home.local/sjspall/compilation-benchmarks/scnum"
echo 1 > $SCNUM # write 1 to file

# create results directories if they don't exist
mkdir -p ${path}/../../ocaml-results
mkdir -p ${path}/../../ocaml-results/rusage-out

# ocaml specific
# need a installation prefix if we aren't super user
installprefix="${path}/../prefix"
mkdir -p ${installprefix}

# clean things up so we are building fresh code.

mkdir -p ${makepath}
cd ${makepath}/..

echo "Cleaning"
rm -rf ${makepath}

echo "un-taring"

tar -xzf ${tarpath}
cd ${makepath}

# another ocaml specific thing
env TMPDIR="${path}/tmp"

echo "building"

./configure --prefix ${installprefix} &>> ${OUTPUTFILE} && strace -f make world &> /data/beehive/home.local/sjspall/straceout.debug && strace -f make install &>> /data/beehive/home.local/sjspall/straceout.debug

export PATH="$oldpath" # restore old path

echo "finished building"
echo "Debugging output written to ${OUTPUTFILE}"


