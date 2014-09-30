type Cell
    genome::BitArray{1}
    flag::Integer
    Cell() = new(trues(GL),1)
end

type Biofilm
    individuals::Array{Cell,1}
    fitness::Float64
    Biofilm() = new([Cell() for i in [1:NC]])
end

type Population
    individuals::Array{Biofilm,1}
    success::Integer
    attempts::Integer
    currentNB::Integer
    Population() = new([Biofilm() for i in [1:NB]], 0, 0, NB)
end
