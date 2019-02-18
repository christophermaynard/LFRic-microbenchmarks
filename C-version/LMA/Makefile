CC=gcc-7
IPATH=../common
VPATH=$(IPATH):.

CFLAGS=-I$(IPATH) -g
LDFLAGS=



%: %.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c 
	$(CC) $(CFLAGS) -c $<

all : lma_driver

clean:
	rm *.o lma_driver 

# dependencies
dino_type.o: dino_type.h
lma_driver.o: dino_type.h matrix_vector_kernel.h
matrix_vector_kernel.o: matrix_vector_kernel.h
lma_driver: lma_driver.o dino_type.o matrix_vector_kernel.o
