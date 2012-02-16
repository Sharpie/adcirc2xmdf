LDFLAGS := -L/usr/local/lib
LDFLAGS += -lnetcdff -lxmdf -lhdf5

FCFLAGS := -Wall -fbounds-check
FCFLAGS += -J/usr/local/include

adcirc2xmdf: adcirc2xmdf.f90 Xmdff.o
	gfortran $(FCFLAGS) $^ -o $@ $(LDFLAGS)

%.o: %.f90
	gfortran -c $<
