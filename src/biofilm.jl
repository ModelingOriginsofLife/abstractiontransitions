Biofilm() = Biofilm(NC)

function getfitness(cells::Array{Cell,1})
    s = reduce((x,y)->x+y, map(x->x.genome, cells))
    s = 1./norm(s-0.99)
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
    new_ = Biofilm(bf.individuals[idx], length(idx))
    grow(new_)
    new_
end

# Returns a set of cells which possess all possible functions
function getspore(bf::Biofilm)
    idx = sample([1:NC], NC, replace=false)
    spore = Array(Integer, 0)
    genepool = Array(BitArray{1}, 0)
    for i in idx
        candidate = bf.individuals[i]
        if isempty(genepool)
            push!(genepool, candidate.genome)
            push!(spore, i)
        elseif sum(map(x->x==candidate.genome, genepool)) == 0
            push!(genepool, candidate.genome)
            push!(spore, i)
        end
        if map(x-> x>0, sum(genepool)) == trues(GL)
            return spore
        end
    end
    println("no possible spore combination ", genepool)
    return 0
end
