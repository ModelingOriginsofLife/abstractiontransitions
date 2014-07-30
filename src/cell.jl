Cell() = Cell(GL)

function getfitness(gen::BitArray{1})
    CFC^sum(gen)
end

function getfitness(c::Cell)
    idx = find(c.genome)

    if length(idx)>1
        eqpairs = map(x->c.promoter[x[1]]==c.promoter[x[2]],
                      combinations(idx,2))
        if length(eqpairs)>0 & sum(eqpairs)>0
            ff = 0.0
        else
            ff = CFC^sum(c.genome)
        end
    else
        ff = CFC^sum(c.genome)
    end

    return ff
end

function mutate(c::Cell)
    idx = [x < y for (x,y) in zip(rand(GL), MF*ones(GL))]
    if !isempty(idx)
        c.genome[idx] = !c.genome[idx]
    end
    idx = find([x < y for (x,y) in zip(rand(GL), PF*ones(GL))])
    if !isempty(idx)
        c.promoter[idx] = rand(1:GL,length(idx))
    end
    c.fitness = getfitness(c)
end

# Display properties of a cell
function display(c::Cell)
    println("Genome: ", c.genome)
    println("Promoter: ", c.promoter)
    println("Fitness: ", c.fitness)
end
