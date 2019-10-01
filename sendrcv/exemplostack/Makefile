FC=mpif90
FFLAGS=-c
LDFLAGS=
SOURCES = $(wildcard *.f95)
OBJECTS = $(subst .f95,.o,$(SOURCES))
EXECUTABLE=exe

.PHONY: clean help exec

all: $(SOURCES) $(EXECUTABLE) exec

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o : %.f95
	$(FC) $(FFLAGS) $<

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

help:
	@echo "Valid targets:"
	@echo "  main.exe"
	@echo "  main.o"
	@echo "  sub1.o"
	@echo "  sub2.o"
	@echo "  clean: removes .o and .exe files"


exec:
	/usr/bin/time --f "%E" /opt/openmpi/bin/mpirun -v -np 2 -report-bindings --hostfile hostfile ./$(EXECUTABLE)
