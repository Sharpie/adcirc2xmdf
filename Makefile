LDFLAGS := -L/usr/local/lib
LDFLAGS += -lnetcdff

FCFLAGS := -Wall -fbounds-check
FCFLAGS += -J/usr/local/include

adcirc2xmdf: adcirc2xmdf.f90
	gfortran $(FCFLAGS) $^ -o $@ $(LDFLAGS)
