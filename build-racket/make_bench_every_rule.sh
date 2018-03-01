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

tstamp=$(date +%T)
outfile="$path/results/${tstamp}_${version}_${machine}.out"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p $(pwd)/results

# create rusage-out directory if it doesnt exist
mkdir -p $path/results/rusage-out

touch $outfile
# write first line to file
echo "version | CPU count | real | user | sys" >> $outfile

mkdir -p ${makepath}

cd ${makepath}/..

echo "Cleaning"

yes | rm -r ${makepath}

echo "un-taring"

tar -xzf ${tarpath}

cd ${makepath}/src

mkdir build

cd build

../configure

echo "running make" 

cpu=1

printsfile="$path/results/rusage-out/${tstamp}_${version}_${machine}_$cpu.out"

touch $printsfile

tout=($( time (make --debug=v SHELL="~/compilation-benchmarks/build-racket/rusage sh" -j $cpu &>> $printsfile ) &&  (make --debug=v SHELL="~/compilation-benchmarks/build-racket/rusage sh" -j $cpu install &>> $printsfile ) 2>&1 )) # parens on outside turn output into an array.

rt=${tout[1]} # real time
ut=${tout[3]} # user time
st=${tout[5]} # sys  time

echo "make done"

# write result to file
echo "$version $cpu $rt $ut $st" >> $outfile

cpu=$interval

while [ "$cpu" -le "$MAX" ] # need the spaces
do
    # clean
    echo "Cleaning"
 
    cd ${makepath}/..

    yes | rm -r ${makepath}

    echo "un-taring"
    tar -xzf ${tarpath}

    cd ${makepath}/src

    mkdir build
    
    cd build

    ../configure
    
    echo "running make"

    printsfile="$path/results/rusage-out/${tstamp}_${version}_${machine}_$cpu.out"

    tout=($( time (make --debug=v SHELL="~/compilation-benchmarks/build-racket/rusage sh" -j $cpu &>> $printsfile) &&  (make --debug=v SHELL="~/compilation-benchmarks/build-racket/rusage sh" -j $cpu install &>> $printsfile ) 2>&1 )) # parens on outside turn output into an array.

    rt=${tout[1]} # real time
    ut=${tout[3]} # user time
    st=${tout[5]} # sys  time

    echo "make done"

    # write result to file
    echo "$version $cpu $rt $ut $st" >> $outfile
    
    cpu=$((cpu + interval))
done

echo "Done"

    

