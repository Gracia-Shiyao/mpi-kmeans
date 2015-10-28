CC = gcc
MCC = mpicc
#FLG = -O4
FC=uhcaf
FFLAGS= -ftpp  
NAME = kmeansTestF
MODS = KMEANS.mod
OBJS = mKmeans.o kmeanTest.o
OBJC = kmeansTest.o kmeans.o 

all: $(OBJS) $(MOD)
	$(FC) $(FFLAGS) $(OBJS) -o $(NAME)
kmeansTestc: $(OBJC) cluster.h
	$(MCC) $(FLG) $(OBJC) -o $@

%.o: %.f90
	$(FC) $(FFLAGS) -c $^ 
%.o: %.c
	$(MCC) $(FLG) -c $^
#kmeanTestF.o: kmeanTestF.f90 $(MODS)
#	$(FC) $(FFLAGS) kmeanTestF.f90 -c
#	
#mKmeansF.o: mKmeansF.f90
#	$(FC) $(FFLAGS) mKmeansF.f90 -c
#
clean:
	rm -f *.o *.mod 
	rm -f *.bin *.out *.exe *.dat
	rm -f $(NAME) kmeansTestc
