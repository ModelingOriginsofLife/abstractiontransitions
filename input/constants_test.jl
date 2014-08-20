#constants
const NB = 5 # number of biofilms
const NC = 20 # number of cells per biofilm
const NP = 10 # number of types of promoter
const GL = 5 # number of genes in a genome
const FF = 5  # number of functions
const MF = 0.001 # gene mutation factor
const PF = 0.01 # promoter mutation factor
const CFC = 0.8 # base of cell fitness function exponent
const MAXTIME = 100 # number of iterations
const FRACMEAS = 10/MAXTIME
const MEASPERIOD = convert(Int64,round(1/FRACMEAS))
const NUMMEAS = convert(Int64,round(MAXTIME/MEASPERIOD) + 1)

#flags
const PLOTFLAG = true
const DEBUGFLAG = false
