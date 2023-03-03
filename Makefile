.PHONY: all clean

lisps := $(shell ffind '\.(asd|lisp)$$')

all: silt

silt: $(lisps)
	sbcl --noinform --disable-debugger --load 'build.lisp' --quit

clean:
	rm silt
