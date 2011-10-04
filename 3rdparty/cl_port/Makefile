#include $(shell rospack find mk)/cmake.mk
DISTNAME=20071106-2

all: install

clean:
	rm -rf asdf
	rm -rf downloaded
	rm -rf cl-port

install: cl-port asdf
	cd asdf;ln -sf ../cl-port_${DISTNAME}/port.asd

asdf:
	mkdir asdf

cl-port: Makefile
#	@echo "you should download https://svncvpr.informatik.tu-muenchen.de/redmine/attachments/19/cl-port_20071106-2.tar.gz"
	tar -xvf cl-port_${DISTNAME}.tar.gz
	touch cl-port

