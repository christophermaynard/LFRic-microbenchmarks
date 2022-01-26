#!-----------------------------------------------------------------------------
#! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
#! For further details please refer to the file LICENCE.original which you
#! should have received as part of this distribution.
#!-----------------------------------------------------------------------------


CC=gcc
IPATH=../common
VPATH=$(IPATH):.

CFLAGS=-I$(IPATH) -g
LDFLAGS=

%: %.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c 
	$(CC) $(CFLAGS) -c $<

all : cma_appinv_driver

clean:
	rm *.o cma_appinv_driver 

# dependencies
array_alloc.o: array_alloc.h
dino_type.o: dino_type.h
cma_appinv_driver.o: dino_type.h  array_alloc.h

cma_appinv_driver: cma_appinv_driver.o dino_type.o array_alloc.o
