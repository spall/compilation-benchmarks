#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
machine=$2
MAX=$3
interval=$4
makepath=$5
tarpath=$6

path=$(pwd)

tstamp=$(date +%s)
outfile="${path}/../../ocaml-results/${tstamp}_${version}_${machine}.csv"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p ${path}/../../ocaml-results

installprefix="${path}/../prefix"

mkdir -p ${installprefix}

touch $outfile
# write first line to file
echo "version, CPU count, real, user, sys" >> $outfile

mkdir -p ${makepath}

cd ${makepath}/..

echo "Cleaning"

rm -rf ${makepath}

echo "un-taring"

tar -xzf ${tarpath}

cd ${makepath}

cpu=1

env TMPDIR="${path}/tmp"
echo "running configure and make"

printsfile="${path}/tmp.out"

./configure --prefix ${installprefix}
tout=($( time ((make -j $cpu world &>> $printsfile ) && (make -j $cpu install &>> $printsfile)) 2>&1 )) # parens on outside turn output into an array.

rt=${tout[1]} # real time
ut=${tout[3]} # user time
st=${tout[5]} # sys  time

echo "make done"

#clean
echo "Cleaning"
 
cd ${makepath}/..
rm -rf ${makepath}
rm -rf ${path}/tmp
rm -rf ${installprefix}

# write result to file
echo "$version, $cpu, $rt, $ut, $st" >> $outfile

cpu=$interval

while [ "$cpu" -le "$MAX" ] # need the spaces
do
    echo "un-taring"
    tar -xzf ${tarpath}

    cd ${makepath}
    
    echo "running configure and make"

    mkdir -p ${path}/tmp

    env TMPDIR="${path}/tmp"

    ./configure --prefix ${installprefix}
    tout=($( time ((make -j 1 world &>> ${printsfile}) && (make -j 1 install &>> ${printsfile})) 2>&1 )) # parens on outside turn output into an array.

    rt=${tout[1]} # real time
    ut=${tout[3]} # user time
    st=${tout[5]} # sys  time

    echo "make done"

    # write result to file
    echo "$version, $cpu, $rt, $ut, $st" >> $outfile
    
    # cleaning
    echo "Cleaning"
 
    cd ${makepath}/..

    rm -rf ${makepath}

    rm -rf ${path}/tmp

    rm -rf ${installprefix}

    cpu=$((cpu + interval))
done

rm -rf ${path}/tmp
rm -f ${path}/tmp.out

echo "Done"

    

