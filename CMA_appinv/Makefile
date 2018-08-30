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

all : cma_appinv_driver

clean:
	rm *.o *.mod cma_appinv_driver

tar:
	tar -cvf dino.saur Makefile *.F90
	gzip dino.saur

# dependencies
constants_mod.o:
dino_mod.o: constants_mod.o
compare_mod.o: constants_mod.o
argument_mod.o: 
kernel_mod.o:
columnwise_op_appinv_kernel_mod.o: constants_mod.o argument_mod.o kernel_mod.o
cma_appinv_driver.o: constants_mod.o dino_mod.o columnwise_op_appinv_kernel_mod.o compare_mod.o
cma_appinv_driver: cma_appinv_driver.o constants_mod.o argument_mod.o dino_mod.o kernel_mod.o columnwise_op_appinv_kernel_mod.o compare_mod.o

