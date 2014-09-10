if length(ARGS) >= 2
    configfile = ARGS[2]
else
    configfile = "constants.jl"
    # configfile = "constants_test.jl"
end

require("include.jl")

#-------------------------
# setup
#-------------------------

if length(ARGS) >= 1
    timestamp = string(gentimestamp(short=true), "_", ARGS[1])
else
    timestamp = gentimestamp()
end

simdir = joinpath(outdir,timestamp)
run(`mkdir $simdir`)
configpath = joinpath(indir,configfile)
run(`cp $configpath $simdir/constants.jl`)

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
       break
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
