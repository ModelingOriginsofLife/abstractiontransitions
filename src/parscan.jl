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

require("main.jl")

using Iterators

ISSs = [1:50]
TRIALs = [1:15]

looptuples = Iterators.product(ISSs,TRIALs)
pmap(x->runsim(x[1],x[2]),looptuples)
