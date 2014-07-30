#constants
const NB = 100 # number of biofilms
const NC = 100 # number of cells per biofilm
const GL = 10 # number of genes in a genome
const FF = 10  # number of functions
const MF = 0.05 # gene mutation factor
const PF = 0.01 # promoter mutation factor
const CFC = 0.8 # base of cell fitness function exponent
const MAXTIME = 1000 # number of iterations
const FRACMEAS = 10/MAXTIME
const MEASPERIOD = convert(Int64,round(1/FRACMEAS))
const NUMMEAS = convert(Int64,round(MAXTIME/MEASPERIOD) + 1)

#flags
const PLOTFLAG = true
