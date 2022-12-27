#!-----------------------------------------------------------------------------
#! Copyright (c) 2022,  Met Office, on behalf of HMSO and Queen's Printer
#! For further details please refer to the file LICENCE.original which you
#! should have received as part of this distribution.
#!-----------------------------------------------------------------------------

FC=gfortran
#FFLAGS=-g -qopenmp -traceback
FFLAGS=-O2
#LDFLAGS=-g -qopenmp -traceback
LDFLAGS=

VPATH=../common:.

%: %.o
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.F90 
	$(FC) $(FFLAGS) -c $<

all : helmholtz_driver

clean:
	rm *.o *.mod helmholtz_driver dino.saur.gz

tar:
	tar -cvf dino.saur Makefile *.F90
	gzip dino.saur

# dependencies
constants_mod.o:
dino_mod.o: constants_mod.o
compare_mod.o: constants_mod.o
argument_mod.o: 
kernel_mod.o:
apply_helmholtz_operator_kernel_mod.o: constants_mod.o argument_mod.o kernel_mod.o
helmholtz_driver.o: constants_mod.o dino_mod.o apply_helmholtz_operator_kernel_mod.o compare_mod.o
helmholtz_driver: helmholtz_driver.o constants_mod.o argument_mod.o dino_mod.o kernel_mod.o apply_helmholtz_operator_kernel_mod.o compare_mod.o

