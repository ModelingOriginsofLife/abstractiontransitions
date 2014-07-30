# types

using Distributions

######################################################
# Definition of the Cell type
# - Cell(), Cell(genomelength)
# - getfitness(genome)
# - mutate(Cell)
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

######################################################
# Definition of the Biofilm type
# - Biofilm(), Biofilm(ncells), Biofilm(CellArray)
# - getfitness(CellArray)
# - grow(Biofilm)
# - display(Biofilm)
# - reproduce(Biofilm)
# - getspore(Biofilm)
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


######################################################
# Definition of the Population type
# - Population(), Population(nfilms)
######################################################
type Population
    individuals::Array{Biofilm,1}

    Population(size::Integer) = new([Biofilm() for i in [1:size]])
end

type Measure
    time::Vector{Int64}
    fitness::Vector{Float64} # average population fitness
    fitnessstd::Vector{Float64} # std population fitness
    numfuncs::Vector{Float64} # average number of functions per cell
    numfuncsstd::Vector{Float64} # std of functions per cell
    diversity::Vector{Float64} # average number of types per biofilm
    diversitystd::Vector{Float64} # std of functions per cell
end

typealias Time Int64
