Population() = Population(NB)

function reproduce(pop::Population)

    fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
    # idx = sample([1:NB], fitvec, NB)
    oldpop = deepcopy(pop)
    i = 1
    while i <= NB
        idx = sample([1:NB], fitvec)
        _new = reproduce(oldpop.individuals[idx])
        if _new == 0
            continue
        else
            pop.individuals[i] = _new
            i += 1
        end
    end

end
