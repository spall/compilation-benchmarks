
all: make submake rusage

make: make.c custom-time.c helper.c
	gcc -o make make.c custom-time.c helper.c

submake: submake.c custom-time.c helper.c
	gcc -o submake submake.c custom-time.c helper.c

rusage: rusage.c custom-time.c helper.c
	gcc -o rusage rusage.c custom-time.c helper.c

clean:
	rm -f make submake rusage

