using StatsBase

require("../test/constants.jl")
require("types.jl")
require("cell.jl")
require("biofilm.jl")
require("population.jl")
require("measure.jl")


# population = Biofilm(5)
# numfuncs = map(x->sum(x.genome),population.individuals)
# totalfuncs = reduce((x,y)->x+y,map(x->x.genome,population.individuals))

#population = Population()
#numfuncs(bf::Biofilm) = map(x->sum(x.genome),bf.individuals)
#totalfuncs(bf::Biofilm) = reduce((x,y)->x+y,map(x->x.genome,bf.individuals))

#map(numfuncs,population.individuals)
#map(totalfuncs,population.individuals)

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

    #measure(pop,meas,t,measnum)
    measnum += 1
end
