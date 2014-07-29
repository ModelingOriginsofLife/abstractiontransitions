#constants
const NB = 5 # number of biofilms
const NC = 20 # number of cells per biofilm
const GL = 5 # number of genes in a genome
const FF = 5  # number of functions
const MF = 0.05 # mutation factor
const MAXTIME = 100 # number of iterations
const FRACMEAS = 10/MAXTIME
const MEASPERIOD = convert(Int64,round(1/FRACMEAS))
const NUMMEAS = convert(Int64,round(MAXTIME/MEASPERIOD) + 1)

#flags
const PLOTFLAG = true
