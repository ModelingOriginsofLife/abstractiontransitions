require("../test/constants.jl")
require("types.jl")

using StatsBase

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

display(pop.individuals[2])

bf = reproduce(pop.individuals[2])
pop.individuals[2] = bf

display(pop.individuals[2])


#while i < NB
    # new_pop = deepcopy(pop)
    # fitvec = (Float64)[x.fitness for x in pop.individuals]
    # idx = sample(WeightVec(fitvec))
    # bf = reproduce(pop.individuals[2])
    # pop.individuals[2] = bf
#end