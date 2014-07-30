using DataFrames

function Measure()
    time = zeros(Int64,NUMMEAS)
    biofitness = zeros(Float64,NUMMEAS)
    fitness = zeros(Float64,NUMMEAS)
    fitnessstd = zeros(Float64,NUMMEAS)
    numfuncs = zeros(Float64,NUMMEAS)
    numfuncsstd = zeros(Float64,NUMMEAS)
    diversity = zeros(Float64,NUMMEAS)
    diversitystd = zeros(Float64,NUMMEAS)

    Measure(time,biofitness,fitness,fitnessstd,numfuncs,numfuncsstd,diversity,diversitystd)
end

function measure(pop::Population, m::Measure, t::Time, n::Int64)
    # Array{Array{Cell,1},1}
    cellsinbiofilms = map(x->x.individuals,pop.individuals)

    # Array{Array{BitArray{1},1},1}
    genomesincells = map(x->map(y->y.genome,x),cellsinbiofilms)

    # store measurements in Measure
    m.time[n] = t
    fitvect = map(x->x.fitness, flatten(cellsinbiofilms))
    numfuncsvect = map(x->sum(x.genome),flatten(cellsinbiofilms))
    diversityvect = map(x->length(unique(x)),genomesincells)

    m.biofitness[n] = mean([bf.fitness for bf in pop.individuals])
    m.fitness[n] = mean(fitvect)
    m.fitnessstd[n] = std(fitvect)
    m.numfuncs[n] = mean(numfuncsvect)
    m.numfuncsstd[n] = std(numfuncsvect)
    m.diversity[n] = mean(diversityvect)
    m.diversitystd[n] = std(diversityvect)

    @printf("time: %06d, measurement: %03d, avg_cfitness: %5.3f, avg_numfuncs: %5.3f avg_diversity: %5.3f, avg_bfitness: %5.3f\n", 
                t, n, m.fitness[n], m.numfuncs[n],m.diversity[n],m.biofitness[n])

end

function save(m::Measure, fname::String)
    df = DataFrame(time=m.time,
                   biofitness=m.biofitness,
                   fitness=m.fitness,
                   fitnessstd=m.fitnessstd,
                   numfuncs=m.numfuncs,
                   numfuncsstd=m.numfuncsstd,
                   diversity=m.diversity,
                   diversitystd=m.diversitystd)
    writetable(fname,df)
    return df
end

# function measure(me::Biofilm)
# # # update fitness, robustness, and modularity measures
# end
