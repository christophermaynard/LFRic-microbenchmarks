#!-----------------------------------------------------------------------------
#! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
#! For further details please refer to the file LICENCE.original which you
#! should have received as part of this distribution.
#!-----------------------------------------------------------------------------


CC=clang
IPATH=../common
VPATH=$(IPATH):.


CFLAGS=-I$(IPATH) -g -O3 -march=cascadelake  -fno-cray-openmp -fopenmp=libomp



LDFLAGS=



%: %.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c 
	$(CC) $(CFLAGS) -c $<

all : lma_driver

clean:
	rm *.o lma_driver 

# dependencies
array_alloc.o: array_alloc.h
dino_type.o: dino_type.h
lma_driver.o: dino_type.h matrix_vector_kernel.h array_alloc.h
matrix_vector_kernel.o: matrix_vector_kernel.h

lma_driver: lma_driver.o dino_type.o matrix_vector_kernel.o array_alloc.o
