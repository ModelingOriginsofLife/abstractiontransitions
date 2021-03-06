using ClusterManagers
addprocs_sge(4)

require("main.jl")
using Iterators

ISSs = [1:2]
TRIALs = [1:2]

looptuples = Iterators.product(ISSs,TRIALs)
pmap(x->runsim(x[1],x[2]),looptuples)
