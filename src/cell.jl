Cell() = Cell(GL)

function getfitness(gen::BitArray{1})
    1-sum(gen)/GL
end

function getfitness(c::Cell)
    idx = find(c.genome)

    if length(idx)>1
        eqpairs = map(x->c.promoter[x[1]]==c.promoter[x[2]],
                      combinations(idx,2))
        if length(eqpairs)>0 & sum(eqpairs)>0
            return 0.0
        end
    end

    return getfitness(c.genome)
end

function mutate(c::Cell)
    # mutate genome
    idx = mutating(GL, MF)
    switchbits(idx, c.genome)
    # mutate promoters)
    idx = mutating(GL, PF)
    if !isempty(idx)
        c.promoter[idx] = rand(1:GL,length(idx))
    end
    c.fitness = getfitness(c)
end

function mutating(n::Int64, f::Float64)
    find([x < y for (x,y) in zip(rand(n), f*ones(n))])
end

function switchbits(idx::Array{Int64}, gen::BitArray{1})
    if !isempty(idx)
        gen[idx] = !gen[idx]
    end
    gen
end

# Display properties of a cell
function display(c::Cell)
    println("Genome: ", c.genome)
    println("Promoter: ", c.promoter)
    println("Fitness: ", c.fitness)
end
