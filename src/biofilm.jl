
function getfitness(bf::Biofilm)
    s = reduce((x,y)->x+y, map(x->x.genome, bf.individuals))
    numberOfFunctions = sum(map(x->x>0, s))
    if numberOfFunctions == GL
        bf.fitness = 1
    else
        bf.fitness = 0
    end
end

# Create a spore and grow it
function reproduce(bf::Biofilm)
    idx = sample([1:NC], ISS, replace=true)
    new_ = Biofilm()
    new_.individuals[1:ISS] = deepcopy(bf.individuals[idx])
    for cell in new_.individuals[1:ISS]
        cell.flag = 0
    end

    idx = sample([1:ISS], NC-ISS, replace=true)
    for (i,j) in enumerate(idx)
        newcell = deepcopy(new_.individuals[j])
        newcell.flag = 0
        new_.individuals[ISS+i] = deepcopy(newcell)
    end

    for cell in new_.individuals
        for i in [1:GL]
            if rand() < MF
                cell.genome[i] = false
            end
        end
    end

    if length(new_.individuals) != NC
        exit("houston we have a problem")
    end

    s = reduce((x,y)->x+y, map(x->x.flag, new_.individuals))
    if s != 0
        exit("flag problem")
    end


    getfitness(new_)
    return new_
end
