Population() = Population(NB)

function reproduce(pop::Population)

    fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
    oldpop = deepcopy(pop)

    trials = 1
    if REPRODUCE == "uniform"
        idx = sample([1:NB], NB)
    elseif REPRODUCE == "fitness"
        idx = sample([1:NB], fitvec, NB)
    elseif REPRODUCE == "duplicate"
        idx = sample([1:NB], NB, replace=false)
        trials = 2
    end
  
    i = 1 # population vector index
    j = 1 # index on idx
    while i <= NB

        if j > length(idx)
            error("index should not be greater than size of idx array.")
            exit()
        end

        k = 1
        while k <= trials
            _new = reproduce(oldpop.individuals[idx[j]])
            pop.attempts += 1
            if _new.fitness == 1
                pop.individuals[i] = _new
                pop.success += 1
                i += 1
                if i > NB
                    break
                end
            end
            k += 1
        end
        j += 1

    end

end
