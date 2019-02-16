all: viewer.py

viewer.py: *.fut
	futhark pyopencl --library viewer.fut

run: viewer.py
	PYOPENCL_CTX=0 ./viewer-gui.py imgs/*.png

lib: futhark.pkg
	futhark pkg sync
