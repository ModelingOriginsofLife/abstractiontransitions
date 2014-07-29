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

# should initialize NB fully developed biofilms
pop = Population(3)

display(pop.individuals[1])

fitvec = (Float64)[x.fitness for x in pop.individuals]

new_gen = sample(pop.individuals, WeightVec(fitvec), length(pop.individuals))
pop.individuals = new_gen