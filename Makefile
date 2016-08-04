.PHONY:

lisps := $(shell ffind '\.(asd|lisp|ros)$$')

vendor/quickutils.lisp: make-quickutils.lisp
	sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

build/silt: $(lisps)
	ros build build/silt.ros
