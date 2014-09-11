.PHONY: default mc parallel plot plotmc clean test

#---------------------------------------------
# Define variables

# list of automatic variables
# http://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
SHELL=/bin/bash
TOPFILE=main.jl
MCFILE=montecarlo.jl
PLOTFILE=plot.jl
PLOTMCFILE=plotmc.jl
TESTFILE=run_tests.jl
PARDEP=include.jl

#---------------------------------------------
# Default target
# will run with
# >make
# alone

default:
ifdef folder
ifdef config
	cd src; julia $(TOPFILE) $(folder) $(config); cd ..
else
	cd src; julia $(TOPFILE) $(folder); cd ..
endif
else
	cd src; julia $(TOPFILE); cd ..
endif

mc:
	cd src; julia $(MCFILE); cd ..

parallel:
	declare -i PNUM; \
	export PNUM=`nproc`-2; \
	cd src; \
	/usr/bin/time -f "%E real\n%U user\n%S sys\n%K memory (kB)" \
	julia -p $(PNUM) -L $(PARDEP) $(TOPFILE); \
	cd ..

plot:
	# run as: make plot simdir=SIMDIR where SIMDIR is the name
	# of a directory in output folder containing results
	# from a previous simulation
	cd src; julia $(PLOTFILE) $(simdir); cd ..

plotmc:
	# run as: make plotmc simdir=SIMDIR where SIMDIR is the name
	# of a directory in output folder containing results
	# from a previous simulation
	cd src; julia $(PLOTMCFILE) $(simdir); cd ..
#----------------------------------------------
# Additional targets

clean:
	rm -rf output/*
	rm input/constants_par*

test:
	julia $(TESTFILE)

#---------------------------------------------
