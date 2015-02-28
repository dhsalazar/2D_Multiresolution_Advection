main: HTYPE.o parameters.o subs.o main.o
	ifort -o main HTYPE.o parameters.o GENESIS.o INTERPOL.o MR.o EVOLUTION.o  subs.o spectral.o main.o -lm -lc -lfftw3 -llapack -L/usr/lib64 -L/opt/fftw3/lib

HTYPE.mod: HTYPE.o HTYPE.f90
	ifort -c HTYPE.f90

HTYPE.o: HTYPE.f90
	ifort -c HTYPE.f90

parameters.mod: HTYPE.mod parameters.o parameters.f90
	ifort -c parameters.f90

parameters.o: HTYPE.mod parameters.f90
	ifort -c parameters.f90


subs.mod: HTYPE.mod parameters.mod  subs.o subs.f90
	ifort -c subs.f90

subs.o: HTYPE.mod parameters.mod  subs.f90
	ifort -c subs.f90


spectral.mod: HTYPE.mod parameters.mod  spectral.o spectral.f90
	ifort -c spectral.f90

spectral.o: HTYPE.mod parameters.mod  spectral.f90
	ifort -c spectral.f90

GENESIS.mod: HTYPE.mod parameters.mod GENESIS.f90
	ifort -c GENESIS.f90

GENESIS.o: HTYPE.mod parameters.mod GENESIS.f90
	ifort -c GENESIS.f90


INTERPOL.mod: HTYPE.mod parameters.mod GENESIS.mod INTERPOL.f90
	ifort -c INTERPOL.f90

INTERPOL.o: HTYPE.mod parameters.mod GENESIS.mod INTERPOL.f90
	ifort -c INTERPOL.f90


MR.mod: HTYPE.mod parameters.mod GENESIS.mod INTERPOL.mod MR.f90
	ifort -c MR.f90

MR.o: HTYPE.mod parameters.mod GENESIS.mod INTERPOL.mod MR.f90
	ifort -c MR.f90

EVOLUTION.mod: HTYPE.mod parameters.mod subs.mod spectral.mod GENESIS.mod INTERPOL.mod EVOLUTION.f90
	ifort -c EVOLUTION.f90

EVOLUTION.o: HTYPE.mod parameters.mod subs.mod spectral. mod GENESIS.mod INTERPOL.mod EVOLUTION.f90
	ifort -c EVOLUTION.f90

main.o: HTYPE.mod parameters.mod GENESIS.mod MR.mod EVOLUTION.mod subs.mod spectral.mod main.f90
	ifort -c main.f90

