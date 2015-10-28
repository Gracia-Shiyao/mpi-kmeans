CC = gcc
MCC = mpicc
#FLG = -O4
FC=uhcaf
FFLAGS= -ftpp  
NAME = kmeansTestF kmeansTestC
MODS = KMEANS.mod
OBJS = mKmeans.o kmeanTest.o
OBJC = kmeansTest.o kmeans.o 


all: $(NAME)

kmeansTestF: $(OBJS) $(MOD)
	$(FC) $(FFLAGS) $(OBJS) -o $@

kmeansTestC: $(OBJC) cluster.h
	$(MCC) $(FLG) $(OBJC) -o $@

%.o: %.f90
	$(FC) $(FFLAGS) -c $^ 
%.o: %.c
	$(MCC) $(FLG) -c $^

clean:
	rm -f *.o *.mod 
	rm -f *.bin *.out *.exe *.dat
	rm -f $(NAME) kmeansTestC
