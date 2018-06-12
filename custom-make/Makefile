
DIR = $(shell pwd)

all: bin/make bin/submake bin/dash

bin/make: make.c custom-time.c helper.c
	gcc -o make make.c custom-time.c helper.c
	mv make bin/make

bin/submake: submake.c custom-time.c helper.c
	gcc -o submake submake.c custom-time.c helper.c
	mv submake bin/submake

bin/dash: 
	cd dash && ./autogen.sh && ./configure --prefix $(DIR) && make && make install

clean:
	rm -f bin/make bin/submake bin/dash


