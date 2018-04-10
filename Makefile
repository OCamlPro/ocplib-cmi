all: build

build: _obuild
	ocp-build

_obuild:
	ocp-build init

clean:
	ocp-build clean

test: build
	./_obuild/ocp-cmi-compress/ocp-cmi-compress.asm --compress-dir _obuild/ocplib-cmi

restore:
	./_obuild/ocp-cmi-compress/ocp-cmi-compress.asm --restore-dir _obuild/ocplib-cmi
