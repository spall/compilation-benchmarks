
DIR = $(shell pwd)

all: bin/make bin/submake bin/actual-make

bin/make: make.c custom-time.c helper.c
	gcc -o make-wrapper make.c custom-time.c helper.c
	mv make-wrapper bin/make

bin/submake: submake.c custom-time.c helper.c
	gcc -o submake submake.c custom-time.c helper.c
	mv submake bin/submake

bin/actual-make: 
	cd make && autoreconf -i && ./configure --prefix=$(DIR)/tmp && make update && make && make install
	cp tmp/bin/make bin/actual-make
	rm -rf tmp

clean:
	rm -f bin/make bin/submake bin/actual-make


