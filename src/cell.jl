Cell() = Cell(GL)

function getfitness(gen::BitArray{1})
    sum(gen)
end

function mutate(c::Cell)
    idx = [x < y for (x,y) in zip(rand(GL), MF*ones(GL))]
    c.genome[idx] = !c.genome[idx]
    c.fitness = getfitness(c.genome)
end

# Display properties of a cell
function display(c::Cell)
    println("Genome: ", c.genome)
    println("Promoter: ", c.promoter)
    println("Fitness: ", c.fitness)
end
