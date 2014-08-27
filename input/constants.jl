#constants
const NB = 100 # number of biofilms
const NC = 100 # number of cells per biofilm

const NG = 1  # initial number of gene per cell
const NP = 10 # number of types of promoter
const GL = 10 # number of genes in a genome

const MF = 0.001 # gene mutation factor
const PF = 0.001 # promoter mutation factor
const CFC = 1.0 # base of cell fitness function exponent
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
             MF,
             PF,
             CFC,
             MAXTIME,
             FRACMEAS,
             MEASPERIOD,
             NUMMEAS,
             PLOTFLAG,
             DEBUGFLAG]
