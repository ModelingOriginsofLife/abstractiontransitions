# types

using Distributions

######################################################
type Cell
    genome::BitArray{1}
    promoter::Array{Integer,1}
    fitness::Float64

    function Cell(genomelength::Integer)
        dist = Categorical([.7,.2,.1])
        numgenes = rand(dist)
        inds = rand(1:genomelength,numgenes)
        genebitstring = falses(genomelength)
        genebitstring[inds] = true
        fitness = getfitness(genebitstring)
        new(genebitstring, rand(1:genomelength,genomelength), fitness)
    end
end

function getfitness(gen::BitArray{1})
    sum(gen)
end

function mutate(c::Cell)
    factor = 0.05
    idx = [x < y for (x,y) in zip(rand(GL), factor*ones(GL))]
    c.genome[idx] = !c.genome[idx]
    c.fitness = getfitness(c.genome)
end


Cell() = Cell(GL)

######################################################
type Biofilm
    individuals::Array{Cell,1}
    fitness::Float64

    function Biofilm(size::Integer) 
        cells = [Cell() for i in [1:size]]
        fitness = getfitness(cells)
        new(cells, fitness)
    end

    function Biofilm(cells::Array{Cell,1})
        fitness = getfitness(cells)
        new(cells, fitness)
    end
end

function getfitness(cells::Array{Cell,1})
    s = reduce((x,y)->x+y, map(x->x.fitness, cells))
    #s *= (0.5 + rand())
    s 
end

function grow(bf::Biofilm)
    ncells = length(bf.individuals)
    for i in range(1, NC-ncells)
        idx = rand(1:ncells + i - 1)
        newcell = deepcopy(bf.individuals[idx])
        mutate(newcell)
        push!(bf.individuals, newcell)
    end
    bf.fitness = getfitness(bf.individuals)
end

function display(bf::Biofilm)
    println("Number of cells: ", length(bf.individuals))
    println("Total fitness: ", bf.fitness)
    for cell in bf.individuals
        println("- ", cell.genome, ", fitness: ",    cell.fitness)
    end
end

function reproduce(bf::Biofilm)
    idx = getspore(bf)
    new_ = Biofilm(bf.individuals[idx])
    grow(new_)
    new_
end

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
    println("spore size ", j)
    return idx[1:j]
end

Biofilm() = Biofilm(NC)

######################################################
type Population
    individuals::Array{Biofilm,1}

    Population(size::Integer) = new([Biofilm() for i in [1:size]])
end

Population() = Population(NB)
