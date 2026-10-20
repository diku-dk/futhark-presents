PROGNAME = futhark-presents
MLKIT    = mlkit
FUTHARK  ?= futhark
SMLFUT   ?= smlfut
SMLPKG   ?= smlpkg
TIGRDIR  = sml/lib/github.com/diku-dk/sml-tigr/clib

# Futhark backend used for the effects: c, multicore, opencl, cuda, or hip.
FUTHARK_BACKEND ?= multicore

# Extra libraries needed by the Futhark backend (comma-terminated).
ifeq ($(FUTHARK_BACKEND),multicore)
  FUT_LIBS = pthread,
else ifeq ($(FUTHARK_BACKEND),opencl)
  FUT_LIBS = OpenCL,
else ifeq ($(FUTHARK_BACKEND),cuda)
  FUT_LIBS = cuda,cudart,nvrtc,
else ifeq ($(FUTHARK_BACKEND),hip)
  FUT_LIBS = hiprtc,amdhip64,
endif

FUTSRC = fut/viewer.fut fut/nbody.fut fut/sand.fut
FUTGEN = fut/viewer.c fut/viewer.h fut/viewer.json
SMLFUTGEN = sml/viewer.sig sml/viewer.sml sml/viewer.smlfut.c

OS = $(shell uname -s)
ARCH = $(shell uname -m)

# Platform detection for linker flags
ifeq ($(OS),Windows_NT)
  LDFLAGS += -s -lopengl32 -lgdi32
else
  ifeq ($(OS),Darwin)
    LDFLAGS += -framework OpenGL -framework Cocoa -L.
    LIBS    += -libdirs . -libs 'viewer,tigr,$(FUT_LIBS)m,c,dl'
    ifeq ($(ARCH),arm64)
      DEPS := libtigr.so libviewer.so
    else
      DEPS := libtigr.a libviewer.a
    endif
  else ifeq ($(OS),Linux)
    DEPS := libtigr.a libviewer.a
    LDFLAGS += -lGLU -lGL -lX11 -lm
    CFLAGS  += -fPIC
    LIBS    += -libdirs . -libs ':libviewer.a,:libtigr.a,GLU,GL,X11,$(FUT_LIBS)m,c,dl'
  endif
endif

.PHONY: all clean realclean prepare

all: prepare $(PROGNAME)

$(TIGRDIR)/tigr.c $(TIGRDIR)/tigr2.c: | prepare
	test -f $@

prepare: sml/lib

# Build the SML program, linking tigr and the Futhark code statically.  The
# archives are passed via -libs (as -l:libfoo.a) so that they appear after the
# MLKit-generated objects.  Do not use --link_code for this: it replaces the
# list of files to link, so the SML program itself would be left out of the
# executable.
$(PROGNAME): sml/$(PROGNAME).mlb sml/slides.sml sml/viewer.sig sml/viewer.sml $(DEPS)
	MLCOMP=mlkit $(MLKIT) -ldexe '$(CC) $(LDFLAGS)' $(LIBS) -o $@ $<

# Build a static archive from the tigr sources directly
tigr.o: $(TIGRDIR)/tigr.c
	$(CC) -c -o $@ $(CFLAGS) -I$(TIGRDIR) $<

tigr2.o: $(TIGRDIR)/tigr2.c
	$(CC) -c -o $@ $(CFLAGS) -I$(TIGRDIR) $<

libtigr.a: tigr.o tigr2.o
	$(AR) rcs $@ $^

sml/lib:
	(cd sml; $(SMLPKG) sync)
	$(MAKE) -C sml/lib/github.com/diku-dk/sml-tigr/clib

libtigr.so: sml/lib
	cp -p sml/lib/github.com/diku-dk/sml-tigr/clib/libtigr.so $@

# Compile the Futhark effects to a C library and generate SML bindings in
# sml/.
$(FUTGEN) &: $(FUTSRC)
	$(FUTHARK) $(FUTHARK_BACKEND) --library fut/viewer.fut

$(SMLFUTGEN) &: fut/viewer.json
	$(SMLFUT) --target=mlkit --structure-name=Viewer -o sml fut/viewer.json

fut/%.o: fut/%.c fut/viewer.h
	$(CC) -c -O3 -o $@ $(CFLAGS) $<

sml/viewer.smlfut.o: sml/viewer.smlfut.c
	$(CC) -c -O3 -o $@ $(CFLAGS) $<

libviewer.a: fut/viewer.o sml/viewer.smlfut.o
	$(AR) rcs $@ $^

libviewer.so: fut/viewer.o sml/viewer.smlfut.o
	$(CC) -shared -o $@ $^ -framework OpenGL -framework Cocoa

clean:
	rm -rf *~ *.exe *.o *.a *.so MLB $(PROGNAME)
	rm -rf $(FUTGEN) $(SMLFUTGEN) fut/*.o sml/*.o sml/MLB

realclean:
	$(MAKE) clean
	rm -rf sml/lib
