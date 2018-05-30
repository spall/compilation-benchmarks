#!/bin/bash

cdir=$PWD

echo "executing top-make: $@ ; in directory $cdir" &>> ${OUTPUTFILE}

ov1=$(date +%s%N)
overhead=$((($(date +%s%N) - ${ov1})))

tsl=$(date +%s%N)

/usr/bin/make --debug=v MAKE="submake" SHELL="rusage /bin/bash" -j ${MAKEJ} "$@" &>> ${OUTPUTFILE} 

tt=$((($(date +%s%N) - ${tsl} - ${overhead})))

echo "topmake-argv= $@\n elapsed= ${tt}\n finishing top-make: $@ ; in directory $cdir" &>> ${OUTPUTFILE}
