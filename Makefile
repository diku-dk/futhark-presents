PROGNAME = futhark-presents
MLKIT    = mlkit
FUTHARK  ?= futhark
SMLFUT   ?= smlfut
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

# Platform detection for linker flags
ifeq ($(OS),Windows_NT)
  LDFLAGS += -s -lopengl32 -lgdi32
else
  UNAME_S := $(shell uname -s)
  ifeq ($(UNAME_S),Darwin)
    LDFLAGS += -arch x86_64 -framework OpenGL -framework Cocoa
    CFLAGS  += -arch x86_64
  else ifeq ($(UNAME_S),Linux)
    LDFLAGS += -lGLU -lGL -lX11 -lm
    CFLAGS  += -fPIC
    LIBS    += -libdirs . -libs ':libviewer.a,:libtigr.a,GLU,GL,X11,$(FUT_LIBS)m,c,dl'
  endif
endif

.PHONY: all clean

all: $(PROGNAME)

# Build the SML program, linking tigr and the Futhark code statically.  The
# archives are passed via -libs (as -l:libfoo.a) so that they appear after the
# MLKit-generated objects.  Do not use --link_code for this: it replaces the
# list of files to link, so the SML program itself would be left out of the
# executable.
$(PROGNAME): sml/$(PROGNAME).mlb sml/slides.sml sml/viewer.sig sml/viewer.sml libtigr.a libviewer.a
	MLCOMP=mlkit $(MLKIT) -ldexe '$(CC) $(LDFLAGS)' $(LIBS) -o $@ $<

# Build a static archive from the tigr sources directly
tigr.o: $(TIGRDIR)/tigr.c
	$(CC) -c -o $@ $(CFLAGS) -I$(TIGRDIR) $<

tigr2.o: $(TIGRDIR)/tigr2.c
	$(CC) -c -o $@ $(CFLAGS) -I$(TIGRDIR) $<

libtigr.a: tigr.o tigr2.o
	$(AR) rcs $@ $^

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

clean:
	rm -rf *~ *.exe *.o *.a MLB $(PROGNAME)
	rm -rf $(FUTGEN) $(SMLFUTGEN) fut/*.o sml/*.o sml/MLB
