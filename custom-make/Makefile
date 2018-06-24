
DIR = $(shell pwd)

all: bin/make bin/submake bin/make-4.2

bin/make: make.c custom-time.c helper.c
	gcc -o make make.c custom-time.c helper.c
	mv make bin/make

bin/submake: submake.c custom-time.c helper.c
	gcc -o submake submake.c custom-time.c helper.c
	mv submake bin/submake

bin/make-4.2: 
	cd make-4.2 && ./configure --prefix=$(DIR)/tmp && make && make install
	cp tmp/bin/make bin/make-4.2
	rm -rf tmp

clean:
	rm -f bin/make bin/submake bin/make-4.2


