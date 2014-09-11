# Loop over parameters
#
# NC
# NG
# NP
# GL
# MF
# PF
# CFC
#
# Run everything with 5000 timesteps, 50 snapshots
# _default: 100, 1, 10, 10, 1e-3, 1e-3, 1
# _NG2: 100, 2, 10, 10, 1e-3, 1e-3, 1
# _NG5: 100, 5, 10, 10, 1e-3, 1e-3, 1
# _NP20: 100, 1, 20, 10, 1e-3, 1e-3, 1
# _CFC2: 100, 1, 10, 10, 1e-3, 1e-3, 2
# _CFC4: 100, 1, 10, 10, 1e-3, 1e-3, 4
# _NC200: 200, 1, 10, 10, 1e-3, 1e-3, 1

NCs = [100,200]
MFs = [1e-2,1e-3]

for NC in NCs, MF in MFs
  partag = "NC$(NC)_MF$(MF)"
  filename = "constants_par$(partag).jl"
  f = open(joinpath("input",filename),"w")

  s = "##################################
  ### Population size, biofilm size
  ##################################

  const NB = 100 # number of biofilms
  const NC = $(NC) # number of cells per biofilm

  ####################
  ### Cell definition
  ####################

  const GL = 10 # number of genes in a genome

  const DIVERSITY = 2 # choices are: \"random\", \"default\", or any integer
  const NG = 1  # initial number of gene per cell, used in \"default\" only.

  const SPORESIZE = \"constant\" # constant, firstPickChooses, complete
  const ISS = 40 # Initial Spore Size

  const USEPROMOTERS = false
  const NP = 10 # number of types of promoter

  #######################
  ### Sampling behaviors
  #######################

  # pick members of the spore based on \"uniform\" or \"fitness\" sampling
  const GETSPORE = \"uniform\"
  # pick duplicating cell based on \"fitness\" or \"uniform\" sampling
  const GROW = \"uniform\"
  # exponent of cell fitness function
  const CFC = 1.0

  # pick reproducing biofilms based on \"fitness\" or \"uniform sampling\"
  # \"duplicate\" allow each biofilm to (try and) reproduce twice in theory.
  # CAUTION: THIS MAY BE POORLY IMPLEMENTED.
  const REPRODUCE = \"duplicate\" # population. based on fitness, uniform, duplicate
  const BFP = 1.0 # Biofilm Fitness Parameter


  ### Mutations
  const MF = $(MF) # gene mutation factor
  const PF = 0.000 # promoter mutation factor
  const HT = 0.000 # horizontal gene transfer

  ######################################################################

  ##################################
  ### Simulation related parameters
  ##################################

  const MAXTIME = 100 # number of iterations
  const FRACMEAS = 50/MAXTIME
  const MEASPERIOD = convert(Int64,round(1/FRACMEAS))
  const NUMMEAS = convert(Int64,round(MAXTIME/MEASPERIOD) + 1)

  ##########
  ### Flags
  ##########

  const PLOTFLAG = false
  const DEBUGFLAG = false

  CONSTANTS = [NB,
               NC,
               NG,
               NP,
               GL,
               ISS,
               MF,
               PF,
               CFC,
               BFP,
               MAXTIME,
               FRACMEAS,
               MEASPERIOD,
               NUMMEAS,
               PLOTFLAG,
               DEBUGFLAG]
"

  write(f,s)
  close(f)

  # run(`cat $(joinpath("input",filename))`)
  spawn(`make folder=NC$(NC)_MF$(MF) configfile=$(filename)`)
  sleep(2)
end

