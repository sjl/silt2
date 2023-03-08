.PHONY: all clean

lisps := $(shell find . '(' -name '*.lisp' -o -name '*.asd' ')')

all: silt

silt: $(lisps)
	sbcl --noinform --disable-debugger --load 'build.lisp' --quit

clean:
	rm silt
