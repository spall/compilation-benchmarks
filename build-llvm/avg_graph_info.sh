#!/bin/bash

# args
version=$1
machine=$2
count=$3
makepath=$4
tarpath=$5

# 1. add custom make script to front of path
path=$(pwd)

oldpath=$PATH # save old path so we can restore when we are done
# 2. set MAKEJ = 1
export MAKEJ="1"

tstamp=$(date +%s)

outdir="${path}/../../llvm-results/rusage-out/${tstamp}"

# 4. set up counter for shell commands
export SCNUM="$path/../scnum"
echo 1 > $SCNUM # write 1 to file

# create results directories if they don't exist
mkdir -p ${path}/../../llvm-results/
mkdir -p ${path}/../../llvm-results/rusage-out
mkdir -p ${outdir}

mkdir -p ${makepath}
cd ${makepath}/..

rm -rf build # remove if it exists

echo "Cleaning"
rm -rf ${makepath}

iters=0

while [ "$iters" -lt "$count" ]
do
    # 3. set output file
    outfile="${outdir}/${iters}_${version}_${machine}.debug" #maybe change extension
    export OUTPUTFILE="${outfile}"
    
    echo "un-taring"
    
    tar -xf ${tarpath}
    cd ${makepath}/..

    mkdir build && cd build
    
    export PATH=/data/home.local/sjspall/projects/cmake-3.12.0-rc1-Linux-x86_64/bin:$PATH

    cmake ${makepath}

    mkdir -p /data/home.local/sjspall/tmp

    export PATH="$path/../custom-make/bin:$PATH"
    echo 1 > $SCNUM

    echo "building"

    make -j 1 #&& cmake -DCMAKE_INSTALL_PREFIX=/data/home.local/sjspall/tmp/llvm -P cmake_install.cmake
        
    echo "make done"
    
    echo "cleaning"
    
    cd ${makepath}/..
    rm -rf ${makepath}
    rm -rf build

    export PATH=$oldpath
    
    iters=$((iters + 1))
done

export PATH="$oldpath" # restore old path
