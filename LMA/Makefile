#!-----------------------------------------------------------------------------
#! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
#! For further details please refer to the file LICENCE.original which you
#! should have received as part of this distribution.
#!-----------------------------------------------------------------------------

FC=ifort
FFLAGS=-g -qopenmp -traceback
#FFLAGS=-g 
LDFLAGS=-g -qopenmp -traceback
#LDFLAGS=-g

VPATH=../common:.

%: %.o
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.F90 
	$(FC) $(FFLAGS) -c $<

all : lma_driver

clean:
	rm *.o *.mod lma_driver dino.saur.gz

tar:
	tar -cvf dino.saur Makefile *.F90
	gzip dino.saur

# dependencies
constants_mod.o:
dino_mod.o: constants_mod.o
compare_mod.o: constants_mod.o
argument_mod.o: 
kernel_mod.o:
matrix_vector_kernel_mod.o: constants_mod.o argument_mod.o kernel_mod.o
lma_driver.o: constants_mod.o dino_mod.o matrix_vector_kernel_mod.o compare_mod.o
lma_driver: lma_driver.o constants_mod.o argument_mod.o dino_mod.o kernel_mod.o matrix_vector_kernel_mod.o compare_mod.o

