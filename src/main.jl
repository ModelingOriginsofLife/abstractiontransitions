require("../test/constants.jl")
require("types.jl")

# population = Biofilm(5)
# numfuncs = map(x->sum(x.genome),population.individuals)
# totalfuncs = reduce((x,y)->x+y,map(x->x.genome,population.individuals))

population = Population()
numfuncs(bf::Biofilm) = map(x->sum(x.genome),bf.individuals)
totalfuncs(bf::Biofilm) = reduce((x,y)->x+y,map(x->x.genome,bf.individuals))

map(numfuncs,population.individuals)
map(totalfuncs,population.individuals)
