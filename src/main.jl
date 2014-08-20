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
# meas = Measure()
meas = MeasureAll()
measnum = 1


for t=1:MAXTIME

    reproduce(pop)

    if maximum( map(x->x.fitness,pop.individuals) ) == 0
       println("No viable biofilm anymore. Saving and exiting.")
       t = MAXTIME
    end

    if (mod(t-1,MEASPERIOD)==0) | (t==MAXTIME)
        measure(pop,meas,t,measnum)
        measnum += 1
    end
end

#-------------------------
# write output and plot
#-------------------------

# df = save(meas,joinpath(simdir,"sim.csv"))
df = save(meas,simdir)

println("------------------------------")
if PLOTFLAG
    plot(simdir)
end

println()
println("view data with:")
outputpath = replace(joinpath(simdir,"sim.csv"),r"\.\./","")
println("column -s, -t < $(outputpath)")
#run(`cat $(joinpath(simdir,"sim.csv"))` |> `column -s, -t`)
