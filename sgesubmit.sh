#!/bin/bash
#$ -S /bin/bash
#$ -N julia-head
#$ -o ~/logs/julia.$JOB_ID.log
#$ -j y
#$ -cwd

cd src/
julia parscan_sge.jl
