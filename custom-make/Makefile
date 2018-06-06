
all: make submake rusage

make: make.c custom-time.c helper.c
	gcc -o make make.c custom-time.c helper.c

submake: submake.c custom-time.c
	gcc -o submake submake.c custom-time.c

rusage: rusage.c custom-time.c helper.c
	gcc -o rusage rusage.c custom-time.c helper.c


