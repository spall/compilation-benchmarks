#!/bin/bash

# version (6.10 etc) | CPU count | rtime | utime | stime

# args
version=$1
machine=$2
MAX=$3
interval=$4
makepath=$5

path=$(pwd)

tstamp=$(date +%T)
outfile="$path/results/${tstamp}_${version}_${machine}.out"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p $path/results

# create directory for build output
mkdir -p $path/results/output

touch $outfile
# write first line to file
echo "version | CPU count | real | user | sys" >> $outfile

cd $makepath

echo "Cleaning"

git clean -fxd     # deletes everything in the git directory that isn't commited. 

echo "running make" 

cpu=1

printsfile="$path/results/output/${tstamp}_${version}_${machine}_$cpu.out" # to store output from make.

tout=($( time (make CPUS=$cpu &>> $printsfile) 2>&1 )) # parens on outside turn output into an array.

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
    git clean -fxd
    echo "running make"

    printsfile="$path/results/output/${tstamp}_${version}_${machine}_$cpu.out"

    tout=($( time (make CPUS=$cpu &>> $printsfile) 2>&1 )) # parens on outside turn output into an array.

    rt=${tout[1]} # real time
    ut=${tout[3]} # user time
    st=${tout[5]} # sys  time

    echo "make done"

    # write result to file
    echo "$version $cpu $rt $ut $st" >> $outfile
    
    cpu=$((cpu + interval))
done

echo "Done"

    

