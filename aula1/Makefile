SOURCES = $(wildcard *.f90)
OBJECTS = $(subst .f90,.o,$(SOURCES))

.PHONY: clean help

main.exe: $(OBJECTS)
	gfortran $(OBJECTS) -o main.exe

%.o : %.f90
	gfortran -c $<

clean:
	rm -f $(OBJECTS) main.exe

help:
	@echo "Valid targets:"
	@echo "  main.exe"
	@echo "  main.o"
	@echo "  sub1.o"
	@echo "  sub2.o"
	@echo "  clean: removes .o and .exe files"
