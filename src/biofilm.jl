Biofilm() = Biofilm(NC)

function getfitness(cells::Array{Cell,1})
    s = reduce((x,y)->x+y, map(x->x.genome, cells))
    s = 1./norm(s-1)
    s
end

# Complete cells a biofilm by duplicating cells of the spore
# A mutation factor is included
function grow(bf::Biofilm)
    ncells = length(bf.individuals)
    for i in range(1, NC-ncells)
        fitvec = WeightVec((Float64)[c.fitness for c in bf.individuals])
        idx = sample([1:length(bf.individuals)], fitvec, 1)
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
        println("- ", cell.genome, ", fitness: ",    cell.fitness)
    end
end

# Create a spore and grow it
function reproduce(bf::Biofilm)
    idx = getspore(bf)
    new_ = Biofilm(bf.individuals[idx])
    grow(new_)
    new_
end

# Returns a set of cells which possess all possible functions
function getspore(bf::Biofilm)
    survival = falses(GL)
    idx = sample([1:NC], NC, replace=false)
    j = 0
    for (j, id) in enumerate(idx)
        survival += bf.individuals[id].genome
        survival = map(x-> x>0, survival)
        if survival == trues(GL)
            break
        end
    end
    if survival != trues(GL)
        println("no possible spore combination ", survival)
    end
    return idx[1:j]
end
