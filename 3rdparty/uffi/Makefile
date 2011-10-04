#include $(shell rospack find mk)/cmake.mk
DISTNAME=2.0.0

all: install

clean:
	rm -rf asdf
	rm -rf downloaded
	rm -rf uffi-${DISTNAME}


install: downloaded asdf
	cd asdf;ln -sf ../uffi-${DISTNAME}/uffi.asd

asdf:
	mkdir asdf

downloaded: Makefile
	wget http://files.b9.com/uffi/uffi-${DISTNAME}.tar.gz
	tar -xvf uffi-${DISTNAME}.tar.gz
	touch downloaded