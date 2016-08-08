.PHONY: deploy update-deps

lisps := $(shell ffind '\.(asd|lisp|ros)$$')

vendor/quickutils.lisp: make-quickutils.lisp
	sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

build/silt: $(lisps)
	ros build build/silt.ros

update-deps:
	hg -R /home/sjl/cl-losh pull -u

/opt/silt/silt: update-deps build/silt
	rm /opt/silt/silt
	cp build/silt /opt/silt/silt

deploy: build/silt
	rsync --exclude=build/silt --exclude=.hg --exclude=silt.prof -avz . silt:/home/sjl/silt2
	ssh silt make -C /home/sjl/silt2 /opt/silt/silt
