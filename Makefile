.PHONY: deploy

lisps := $(shell ffind '\.(asd|lisp|ros)$$')

vendor/quickutils.lisp: make-quickutils.lisp
	sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

build/silt: $(lisps)
	ros build build/silt.ros

/opt/silt/silt: build/silt
	cp build/silt /opt/silt/silt

deploy:
	rsync --exclude=build/silt --exclude=.hg -avz . silt:/home/sjl/silt2
	ssh silt make -C /home/sjl/silt2 /opt/silt/silt
