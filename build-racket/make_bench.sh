#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
MAX=$2
interval=$3
makepath=$4

tstamp=$(date +%T)
outfile="$(pwd)/results/$1_$tstamp.out"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p $(pwd)/results

touch $outfile
# write first line to file
echo "version | CPU count | real | user | sys" >> $outfile

cd $makepath

cpu=1
while [ "$cpu" -le "$MAX" ] # need the spaces
do
    # clean
    echo "Cleaning"
    
    rm -r build
    rm -r racket/src/build

    echo "running make"

    tout=($( time (make CPUS=$cpu 2>/dev/null 1>&2 ) 2>&1 )) # parens on outside turn output into an array.

    rt=${tout[1]} # real time
    ut=${tout[3]} # user time
    st=${tout[5]} # sys  time

    echo "make done"

    # write result to file
    echo "$version $cpu $rt $ut $st" >> $outfile
    
    cpu=$((cpu + interval))
done

echo "Done"

    

