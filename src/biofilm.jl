using Distance

Biofilm() = Biofilm(NC)

function getfitness(cells::Array{Cell,1})

    # compute average normalized cosine similarity
    # wp:Cosine_similarity#Angular_similarity
    d = reduce((x,y)->hcat(x,y),map(x->int(x.genome),cells))
    if size(d,2)>1
        d = sum(1-2*acos(1-pairwise(CosineDist(),d))/pi)/(NC^2)
    else
        d = 1
    end

    # check if all functions are available
    # and assign average normalized cosine similarity if so
    # or 0.0 otherwise
    s = reduce((x,y)->x+y, map(x->x.genome, cells))
    if map(x->x>0,s) == trues(GL)
        # s = 1./norm(s-0.99)
        s = d
    else
        s = 0.0
    end
    s
end

# Complete cells a biofilm by duplicating cells of the spore
# A mutation factor is included
function grow(bf::Biofilm)
    ncells = length(bf.individuals)
    for i in range(1, NC-ncells)
        # horizontal gene transfer
        if rand() < 5*MF
            idx = sample([1:length(bf.individuals)],2)
            gidx = sample([1:GL])
            bf.individuals[idx[1]].genome[gidx] = bf.individuals[idx[2]].genome[gidx]
        end

        # replicate and mutate
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
    if DEBUGFLAG
        println("no possible spore combination ", sum(genepool))
    end
    return 0
end
