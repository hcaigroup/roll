#include $(shell rospack find mk)/cmake.mk
DISTNAME=5.3.3

all: install

clean:
	rm -rf asdf
	rm -rf downloaded
	rm -rf clsql-${DISTNAME}


install: downloaded asdf
	cd asdf;ln -sf ../clsql-${DISTNAME}/clsql.asd
	cd asdf;ln -sf ../clsql-${DISTNAME}/clsql-mysql.asd
	cd asdf;ln -sf ../clsql-${DISTNAME}/clsql-uffi.asd

asdf:
	mkdir asdf

downloaded: Makefile
	#wget https://svncvpr.informatik.tu-muenchen.de/redmine/attachments/21/clsql-${DISTNAME}.tar.gz
	tar -xvf clsql-${DISTNAME}.tar.gz
	touch downloaded


