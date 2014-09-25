#!/bin/bash
#$ -S /bin/bash
#$ -N julia-head
#$ -o $HOME/logs/julia.$JOB_ID.log
#$ -j y
#$ -cwd

cd src/
PATH=$PATH:/opt/gridengine/bin/lx26-amd64
/apps1/julia/0.4.0-dev/gnu/bin/julia parscan_sge.jl
