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
    expressed::BitArray{1}
    fitness::Float64
    wantSporeSize::Integer

    function Cell(genomelength::Integer)
        # dist = Categorical([.7,.2,.1])
        # numgenes = rand(dist)
        inds = rand(1:genomelength, NG)
        genebitstring = falses(genomelength)
        genebitstring[inds] = true
        promoter = convert(Array{Integer}, rand(1:NP, genomelength))
        expressed = getexpressed(genebitstring, promoter)

        fitness = getfitness(genebitstring)

        new(genebitstring, promoter, expressed, fitness, ISS)
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
    sporesize::Integer

    function Biofilm(size::Integer)
        cells = [Cell() for i in [1:size]]
        fitness = getfitness(cells)
        new(cells, fitness, NC/GL)
    end

    function Biofilm(cells::Array{Cell,1},sporesize)
        fitness = getfitness(cells)
        new(cells, fitness, sporesize)
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
    biofitness::Vector{Float64} # average population fitness
    biofitnessstd::Vector{Float64} # average population fitness
    fitness::Vector{Float64} # average population fitness
    fitnessstd::Vector{Float64} # std population fitness
    numfuncs::Vector{Float64} # average number of functions per cell
    numfuncsstd::Vector{Float64} # std of functions per cell
    diversity::Vector{Float64} # average number of types per biofilm
    diversitystd::Vector{Float64} # std of functions per cell
    sporesize::Vector{Float64} # average sporesize for biofilms
    sporesizestd::Vector{Float64} # std of sporesize for biofilms
    expressed::Vector{Float64} # average sporesize for biofilms
    expressedstd::Vector{Float64} # std of sporesize for biofilms
end

type MeasureAll
    time::Vector{Int64}
    biofitness::Matrix{Float64} # population fitness
    fitness::Matrix{Float64} # population fitness
    numfuncs::Matrix{Float64} # number of functions per cell
    diversity::Matrix{Float64} # number of types per biofilm
    sporesize::Matrix{Float64} # sporesize for biofilms
    expressed::Matrix{Float64} # sporesize for biofilms
end

typealias Time Int64
