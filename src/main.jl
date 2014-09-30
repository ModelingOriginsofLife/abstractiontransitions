
using StatsBase

require("../input/constants.jl")
require("types.jl")
require("cell.jl")
require("biofilm.jl")
require("population.jl")

println("------------------------------")

pop = Population()

for t=1:MAXTIME

  reproduce(pop)

  if maximum( map(x->x.fitness,pop.individuals) ) == 0 || pop.currentNB == 0
    println("No viable biofilm anymore. Saving and exiting.")
    break
  end

  if mod(t,100) == 0
    println("---------")
    println(pop.success/pop.attempts)
  end

end

println("------------------------------")

