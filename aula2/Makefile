FC=gfortran -O3
FFLAGS=-c
LDFLAGS=
SOURCES = $(wildcard *.f90)
OBJECTS = $(subst .f90,.o,$(SOURCES))
EXECUTABLE=demo

.PHONY: clean help exec

all: $(SOURCES) $(EXECUTABLE) exec

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o : %.f90
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
	./demo
