FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan
FOPT=
#FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/mod_io.f90 -J obj/ -o obj/mod_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/mod_sort.f90 -J obj/ -o obj/mod_sort.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/mod_io.o ./obj/mod_sort.o ./obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/Out.txt
	cat bin/Out_len.txt
