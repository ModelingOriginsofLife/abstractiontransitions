Population() = Population(NB)

function reproduce(pop::Population)

    fitvec = WeightVec((Float64)[bf.fitness for bf in pop.individuals])
    oldpop = deepcopy(pop)

    tmpPop = Array(Biofilm, 0)
    if REPRODUCE == "duplicate"
        for bf in oldpop.individuals
            k = 1
            while k <= 2
                trial = reproduce(bf)
                if trial.fitness == 1.
                    push!(tmpPop, trial)
                    pop.success += 1
                end
                k = k + 1
                pop.attempts += 1
            end
        end
    end

    i = 1
    while i <= NB
        if REPRODUCE == "uniform"
            idx = sample([1:NB])
        elseif REPRODUCE == "fitness"
            idx = sample([1:NB], fitvec)
        elseif REPRODUCE == "duplicate"
            pop.individuals[i] = tmpPop[rand(1:length(tmpPop))]
            i += 1
            continue
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
