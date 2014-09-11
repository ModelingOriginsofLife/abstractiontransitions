using Distance

Biofilm() = Biofilm(NC)

function getfitness(cells::Array{Cell,1})
    s = reduce((x,y)->x+y, map(x->x.expressed, cells))
    numberOfFunctions = sum(map(x->x>0, s))
    fitness = exp(-BFP*(GL-numberOfFunctions))
    return fitness
end


# Complete cells a biofilm by duplicating cells of the spore
# A mutation factor is included
function grow(bf::Biofilm)
    ncells = length(bf.individuals)
    for i in range(1, NC-ncells)
        # horizontal gene transfer
        if rand() < HT
            idx = sample([1:length(bf.individuals)], 2)
            gidx = sample([1:GL])
            bf.individuals[idx[1]].genome[gidx] = bf.individuals[idx[2]].genome[gidx]
        end

        # replicate and mutate
        if GROW == "fitness"
            fitvec = WeightVec((Float64)[c.fitness for c in bf.individuals])
            idx = sample([1:length(bf.individuals)], fitvec, 1)
        elseif GROW == "uniform"
            idx = sample([1:length(bf.individuals), 1])
        end

        newcell = deepcopy(bf.individuals[idx[1]])
        mutate(newcell)
        push!(bf.individuals, newcell)
    end
    bf.fitness = getfitness(bf.individuals)
end

# Display properties of a biofilm
function display(bf::Biofilm)
    println("Number of cells: ", length(bf.individuals))
    println("Total fitness: ", bf.fitness)
    for cell in bf.individuals
        println("- ", cell.genome, " ", cell.promoter, " ", cell.expressed, ", fitness: ", cell.fitness)
    end
end

# Create a spore and grow it
function reproduce(bf::Biofilm)

    idx = getspore(bf)
    if idx == 0
        return 0
    else
        new_ = Biofilm(bf.individuals[idx], length(idx))
        grow(new_)
        return new_
    end
end

# Returns a set of cells which possess all possible functions
function getspore(bf::Biofilm)
    spore = Array(Integer, 0)
    genepool = Array(BitArray{1}, 0)

    if GETSPORE == "uniform"
        idx = sample([1:NC], NC, replace=false)
    elseif GETSPORE == "fitness"
        fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
        idx = sample([1:NC], fitvec, replace=false)
    end

    for i in idx
        candidate = bf.individuals[i]
        sporeSize = candidate.wantSporeSize
        push!(genepool, candidate.genome)
        push!(spore, i)
        case1 = (length(spore) == sporeSize) && (SPORESIZE == "firstPickChooses")
        case2 = (length(spore) == ISS) && (SPORESIZE == "constant")
        case3 = (map(x-> x>0, sum(genepool)) == trues(GL) && (SPORESIZE == "complete"))
        if case1 || case2 || case3
            return spore
        end
    end
    if DEBUGFLAG
        println("Something fishy in getspore(), we should not be here.")
    end
    return 0
end
