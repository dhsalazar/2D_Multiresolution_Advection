main: HTYPE.o parameters.o subs.o main.o
	gfortran -o main HTYPE.o parameters.o GENESIS.o INTERPOLATION.o MR.o EVOLUTION.o  subs.o  main.o 

HTYPE.mod: HTYPE.o HTYPE.f90
	gfortran -c HTYPE.f90

HTYPE.o: HTYPE.f90
	gfortran -c HTYPE.f90

parameters.mod: HTYPE.mod parameters.o parameters.f90
	gfortran -c parameters.f90

parameters.o: HTYPE.mod parameters.f90
	gfortran -c parameters.f90


subs.mod: HTYPE.mod parameters.mod  subs.o subs.f90
	gfortran -c subs.f90

subs.o: HTYPE.mod parameters.mod  subs.f90
	gfortran -c subs.f90


GENESIS.mod: HTYPE.mod parameters.mod GENESIS.f90
	gfortran -c GENESIS.f90

GENESIS.o: HTYPE.mod parameters.mod GENESIS.f90
	gfortran -c GENESIS.f90


INTERPOLATION.mod: HTYPE.mod parameters.mod GENESIS.mod INTERPOLATION.f90
	gfortran -c INTERPOLATION.f90

INTERPOLATION.o: HTYPE.mod parameters.mod GENESIS.mod INTERPOLATION.f90
	gfortran -c INTERPOLATION.f90


MR.mod: HTYPE.mod parameters.mod GENESIS.mod INTERPOLATION.mod MR.f90
	gfortran -c MR.f90

MR.o: HTYPE.mod parameters.mod GENESIS.mod INTERPOLATION.mod MR.f90
	gfortran -c MR.f90

EVOLUTION.mod: HTYPE.mod parameters.mod subs.mod GENESIS.mod INTERPOLATION.mod EVOLUTION.f90
	gfortran -c EVOLUTION.f90

EVOLUTION.o: HTYPE.mod parameters.mod subs.mod GENESIS.mod INTERPOLATION.mod EVOLUTION.f90
	gfortran -c EVOLUTION.f90

main.o: HTYPE.mod parameters.mod GENESIS.mod MR.mod EVOLUTION.mod subs.mod main.f90
	gfortran -c main.f90

