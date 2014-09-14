##################################
  ### Population size, biofilm size
  ##################################

  const NB = 100 # number of biofilms
  const NC = 100 # number of cells per biofilm

  ####################
  ### Cell definition
  ####################

  const GL = 10 # number of genes in a genome

  const DIVERSITY = "default" # choices are: "random", "default", or any integer
  const NG = 10  # initial number of gene per cell, used in "default" only.

  const SPORESIZE = "constant" # constant, firstPickChooses, complete
  const ISS = 40 # Initial Spore Size

  const USEPROMOTERS = false
  const NP = 10 # number of types of promoter

  #######################
  ### Sampling behaviors
  #######################

  # pick members of the spore based on "uniform" or "fitness" sampling
  const GETSPORE = "uniform"
  # pick duplicating cell based on "fitness" or "uniform" sampling
  const GROW = "uniform"
  # exponent of cell fitness function
  const CFC = 1.0

  # pick reproducing biofilms based on "fitness" or "uniform sampling"
  # "duplicate" allow each biofilm to (try and) reproduce twice in theory.
  # CAUTION: THIS MAY BE POORLY IMPLEMENTED.
  const REPRODUCE = "duplicate" # population. based on fitness, uniform, duplicate
  const BFP = 1.0 # Biofilm Fitness Parameter


  ### Mutations
  const MF = 0.010 # gene mutation factor
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
