# types

using Distributions

type Cell
    genome::BitArray{1}
    promoter::Array{Integer,1}

    function Cell(genomelength::Integer)
        dist = Categorical([.7,.2,.1])
        numgenes = rand(dist)
        inds = rand(1:genomelength,numgenes)
        genebitstring = falses(genomelength)
        genebitstring[inds] = true
        return new(genebitstring, rand(1:genomelength,genomelength))
    end
end

Cell() = Cell(GL)

type Biofilm
    individuals::Array{Cell,1}

    Biofilm(size::Integer) = new([Cell() for i in [1:size]])
end

Biofilm() = Biofilm(NC)

type Population
    individuals::Array{Biofilm,1}

    Population(size::Integer) = new([Biofilm() for i in [1:size]])
end

Population() = Population(NB)
