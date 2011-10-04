#include $(shell rospack find mk)/cmake.mk
DISTNAME=0.0.7

all: install

clean:
	rm -rf asdf
	rm -rf cl-libsvm-${DISTNAME}.tar.gz
	rm -rf cl_libsvm
	rm -rf downloaded

install: asdf buildc
	cd asdf;ln -sf ../src/cl-libsvm.asd

asdf:
	mkdir asdf

buildc:
	cd src/lib/src;make


# obsolete, using ROS patched sources
# downloaded: Makefile
# 	wget http://quotenil.com/binary/cl-libsvm/cl-libsvm-${DISTNAME}.tar.gz
# 	tar -xvf cl-libsvm-${DISTNAME}.tar.gz
# 	touch downloaded