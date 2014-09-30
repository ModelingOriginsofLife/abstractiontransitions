function reproduce(pop::Population)

    oldpop = deepcopy(pop)

    idx = sample([1:pop.currentNB], pop.currentNB, replace=true)

    trials = 2
    i = 1 # population vector index
    j = 1 # index on idx

    while (i <= NB && j <= pop.currentNB)
        k = 1
        while k <= trials
            _new = reproduce(oldpop.individuals[idx[j]])
            pop.attempts += 1
            if _new.fitness > 0.5
                pop.individuals[i] = deepcopy(_new)
                pop.success += 1
                i += 1 # we filled one spot in the population vector, prepare next
                if i > NB
                    break
                end
            end
            k += 1 # next trial for a given biofilm
        end
        j += 1 # try next biofilm in ordered population
    end

    pop.currentNB = i-1
    println(pop.currentNB)

end
