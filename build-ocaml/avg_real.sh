#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
machine=$2
count=$3
makepath=$4
tarpath=$5

path=$(pwd)

tstamp=$(date +%s)
outfile="${path}/../../ocaml-results/${tstamp}_${version}_${machine}.csv"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p ${path}/../../ocaml-results

installprefix="${path}/../prefix"

touch $outfile
# write first line to file
echo "work (nanoseconds), user (seconds), sys (seconds)" >> $outfile

mkdir -p ${makepath}

cd ${makepath}/..

echo "Cleaning"

rm -rf ${makepath}
rm -rf ${installprefix}

printsfile="${path}/tmp.out"

iters=0

while [ "$iters" -lt "$count" ]
do
    
    echo "un-taring"
    tar -xf ${tarpath}
    
    cd ${makepath}
    
    env TMPDIR="${path}/tmp"
    
    echo "running configure and make"
    
    ./configure --prefix ${installprefix}
    ov1=$(date +%s%N) && overhead=$((($(date +%s%N) - ${ov1})))
    
    tout=($( time (tsl=$(date +%s%N) && make -j 1 world &>> ${printsfile} && make -j 1 install &>> ${printsfile} && tt=$((($(date +%s%N) - ${tsl} - ${overhead}))) && printf "%d, " $tt >> $outfile) 2>&1))

    ut=${tout[3]} # user time
    st=${tout[5]} # sys  time
    
    echo "make done"
    
    # write result to file
    echo "$ut, $st" >> $outfile
    
    # cleaning
    echo "Cleaning"
    
    cd ${makepath}/..
    
    rm -rf ${makepath}
    
    rm -rf ${path}/tmp
    rm -rf ${installprefix}
    
    iters=$((iters + 1))

done

rm -f ${path}/tmp.out
rm -rf ${path}/tmp

echo "Done"

    

