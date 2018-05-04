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
outfile="${path}/../../racket-results/${tstamp}_${version}_${machine}.csv"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p ${path}/../../racket-results

touch $outfile
# write first line to file
echo "version, CPU count, real, user, sys" >> $outfile

mkdir -p ${makepath}

cd ${makepath}/..

echo "Cleaning"

rm -rf ${makepath}

echo "un-taring"

tar -xzf ${tarpath}

echo "Replacing broken rktio_process.c"

cp rktio_process.c ${makepath}/src/rktio/

cd ${makepath}/src

mkdir build

cd build

echo "running configure and make" 

cpu=1

printsfile="${path}/tmp.out"

tout=($( time ((../configure &>> $printsfile) && (make PLT_SETUP_OPTIONS="-j $cpu" -j $cpu &>> $printsfile ) && (make PLT_SETUP_OPTIONS="-j $cpu" -j $cpu install &>> $printsfile)) 2>&1 )) # parens on outside turn output into an array.

rt=${tout[1]} # real time
ut=${tout[3]} # user time
st=${tout[5]} # sys  time

echo "make done"

# write result to file
echo "$version, $cpu, $rt, $ut, $st" >> $outfile

echo "Cleaning"
 
cd ${makepath}/..

rm -rf ${makepath}

cpu=$interval

while [ "$cpu" -le "$MAX" ] # need the spaces
do
    echo "un-taring"
    tar -xzf ${tarpath}

    cp rktio_process.c ${makepath}/src/rktio/

    cd ${makepath}/src

    mkdir build
    
    cd build
    
    echo "running configure and make"
    
    tout=($( time ((../configure &>> ${printsfile}) && (make PLT_SETUP_OPTIONS="-j $cpu" -j $cpu &>> ${printsfile}) && (make PLT_SETUP_OPTIONS="-j $cpu" -j $cpu install &>> ${printsfile})) 2>&1 )) # parens on outside turn output into an array.

    rt=${tout[1]} # real time
    ut=${tout[3]} # user time
    st=${tout[5]} # sys  time

    echo "make done"

    # write result to file
    echo "$version, $cpu, $rt, $ut, $st" >> $outfile
    
    echo "Cleaning"
 
    cd ${makepath}/..

    rm -rf ${makepath}
    
    cpu=$((cpu + interval))
done

rm -f ${path}/tmp.out

echo "Done"

    

