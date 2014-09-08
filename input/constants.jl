###################################
######## CHOOSE MODEL TYPE ########
###################################

# CELL TYPE
const USEPROMOTERS = false
const CELLFITNESS = 'uniform' # choices are: uniform, type1, type2

# BIOFILM TYPE
const DIVERSITY = 'random' # choices are: random, homogeneous, or any integer
const BIOFITNESS = 'uniform' # choices are: uniform, type1, type2

# DYNAMICS
const GROWTH = 'fitness' # fitness or uniform
const GETSPORE = 'uniform' # uniform, constant, varying, complete
const REPRODUCE = 'fitness' # fitness, uniform, duplicate

###################################
###### CHOOSE MODEL CONSTANTS #####
###################################

#constants
const NB = 100 # number of biofilms
const NC = 100 # number of cells per biofilm

const NG = 1  # initial number of gene per cell
const NP = 10 # number of types of promoter
const GL = 10 # number of genes in a genome
const ISS = 40 # Initial Spore Size

const MF = 0.001 # gene mutation factor
const PF = 0.001 # promoter mutation factor

const CFC = 1.0 # base of cell fitness function exponent
const BFP = 1.0 # Biofilm Fitness Parameter

const MAXTIME = 5000 # number of iterations
const FRACMEAS = 50/MAXTIME
const MEASPERIOD = convert(Int64,round(1/FRACMEAS))
const NUMMEAS = convert(Int64,round(MAXTIME/MEASPERIOD) + 1)

#flags
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
