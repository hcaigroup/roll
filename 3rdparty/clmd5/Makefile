#include $(shell rospack find mk)/cmake.mk
DISTNAME=1.8.5

all: install

clean:
	rm -rf asdf
	rm -rf downloaded
	rm -rf md5-${DISTNAME}


install: downloaded asdf
	cd asdf;ln -sf ../md5-${DISTNAME}/md5.asd

asdf:
	mkdir asdf

downloaded: Makefile
	wget http://files.b9.com/md5/md5-${DISTNAME}.tar.gz
	tar -xvf md5-${DISTNAME}.tar.gz
	touch downloaded