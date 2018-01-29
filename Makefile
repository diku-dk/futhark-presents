all: viewer.py

viewer.py: viewer.fut
	futhark-pyopencl --library viewer.fut

run: viewer.py
	PYOPENCL_CTX=0 ./viewer-gui.py imgs/*.png
