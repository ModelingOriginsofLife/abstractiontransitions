using StatsBase
using Datetime

require("../test/constants.jl")
require("utilities.jl")
require("types.jl")
require("cell.jl")
require("biofilm.jl")
require("population.jl")
require("measure.jl")

#-------------------------
# setup
#-------------------------

# configfile = "constants.jl"
configfile = "constants_test.jl"
indir = joinpath("..","input")
outdir = joinpath("..","output")
require(joinpath(indir,configfile))

timestamp = gentimestamp()

simdir = joinpath(outdir,timestamp)
run(`mkdir $simdir`)
configpath = joinpath(indir,configfile)
run(`cp $configpath $simdir`)

#----------------------------------

pop = Population()
meas = Measure()
measnum = 1


for t=1:MAXTIME
    println("-- ", t)

    fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
    println(fitvec)
    idx = sample([1:NB], fitvec, NB)

    for i in idx
        _new = reproduce(pop.individuals[i])
        pop.individuals[i] = _new
    end

    measure(pop,meas,t,measnum)
    measnum += 1
end

#-------------------------
# write output
#-------------------------

df = save(meas,joinpath(simdir,"sim.csv"))
println()
#run(`cat $(joinpath(simdir,"sim.csv"))` |> `column -s, -t`)
println("\nview output with:")
outputpath = replace(joinpath(simdir,"sim.csv"),r"\.\./","")
println("column -s, -t < $(outputpath)")
#----------------------------------

if PLOTFLAG

    plotspdf = joinpath(simdir,"plots.pdf")
    plotxvar = "time"
    plotyvar = ["fitness", "numfuncs"]
    plotspdfs = map(x->joinpath(simdir,string(x,".pdf")),plotyvar)
    run(`python plotdata.py -d $simdir -x $plotxvar -y $plotyvar`)
    run(`gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=$plotspdf
            -dBATCH $plotspdfs`)
    spawn(`evince $plotspdf`)

end
