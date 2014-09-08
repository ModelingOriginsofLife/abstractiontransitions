Population() = Population(NB)

function reproduce(pop::Population)

    fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
    # idx = sample([1:NB], fitvec, NB)
    oldpop = deepcopy(pop)
    i = 1
    while i <= NB
        if REPRODUCE == "uniform"
            idx = sample([1:NB], 1)
        elseif REPRODUCE == "fitness"
            idx = sample([1:NB], fitvec)
        elseif REPRODUCE == "duplicate"
            # not sure this is actually the correct way to do it
            idx = sample(vcat([1:NB], [1:NB]), 1)
        end
        _new = reproduce(oldpop.individuals[idx])
        if _new == 0
            continue
        else
            pop.individuals[i] = _new
            i += 1
        end
    end

end
