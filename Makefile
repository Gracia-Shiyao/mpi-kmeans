#CC = gcc
#MCC = mpicc
#FLG = -O4
FC=uhcaf
FFLAGS= -ftpp -DDEBUG
NAME = kmeanTest
MODS = KMEANS.mod
OBJS = mKmeans.o kmeanTest.o

all: $(OBJS) $(MOD)

	$(FC) $(FFLAGS) $(OBJS) -o $(NAME)

kmeanTest.o: kmeanTest.f90 $(MODS)

	$(FC) $(FFLAGS) kmeanTest.f90 -c
	
mKmeans.o: mKmeans.f90

	$(FC) $(FFLAGS) mKmeans.f90 -c

clean:
	rm -f *.o *.mod 
