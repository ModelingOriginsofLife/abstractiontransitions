using StatsBase
using Datetime

require("include.jl")

#-------------------------
# setup
#-------------------------

timestamp = gentimestamp()

simdir = joinpath(outdir,timestamp)
run(`mkdir $simdir`)
configpath = joinpath(indir,configfile)
run(`cp $configpath $simdir`)

#----------------------------------
println("------------------------------")

pop = Population()
meas = Measure()
measnum = 1


for t=1:MAXTIME

    fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
    idx = sample([1:NB], fitvec, NB)

    for i in idx
        _new = reproduce(pop.individuals[i])
        pop.individuals[i] = _new
    end

    if (mod(t-1,MEASPERIOD)==0) | (t==MAXTIME)
        measure(pop,meas,t,measnum)
        measnum += 1
    end
end

#-------------------------
# write output and plot
#-------------------------

df = save(meas,joinpath(simdir,"sim.csv"))

println("------------------------------")
if PLOTFLAG
    plot(simdir)
end

println()
println("view data with:")
outputpath = replace(joinpath(simdir,"sim.csv"),r"\.\./","")
println("column -s, -t < $(outputpath)")
#run(`cat $(joinpath(simdir,"sim.csv"))` |> `column -s, -t`)
