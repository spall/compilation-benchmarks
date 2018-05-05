#!/bin/sh

echo "executing top-make: $@ ; in directory $PWD" &>> ${OUTPUTFILE}

time -f 'topmake-argv= %C\n rc=%x elapsed=%e user=%U system=%S maxrss=%M avgrss=%t ins=%I outs=%O minflt=%R majflt=%F swaps=%W avgmem=%K avgdata=%D\n' /usr/bin/make --debug=v MAKE="submake" SHELL="rusage /bin/bash" -j ${MAKEJ} "$@" &>> ${OUTPUTFILE} 

echo "finishing top-make: $@ ; in directory $PWD" &>> ${OUTPUTFILE}
