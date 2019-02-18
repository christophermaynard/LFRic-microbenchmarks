CC=pgcc
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
lma_driver.o: dino_type.h
lma_driver: lma_driver.o dino_type.o
