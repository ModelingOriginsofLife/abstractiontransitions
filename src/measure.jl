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
    sporesize = zeros(Float64,NUMMEAS)
    sporesizestd = zeros(Float64,NUMMEAS)

    Measure(time,biofitness,fitness,fitnessstd,numfuncs,numfuncsstd,diversity,diversitystd,sporesize,sporesizestd)
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
    sporesizevect = map(x->x.sporesize,pop.individuals)

    m.biofitness[n] = mean([bf.fitness for bf in pop.individuals])
    m.fitness[n] = mean(fitvect)
    m.fitnessstd[n] = std(fitvect)
    m.numfuncs[n] = mean(numfuncsvect)
    m.numfuncsstd[n] = std(numfuncsvect)
    m.diversity[n] = mean(diversityvect)
    m.diversitystd[n] = std(diversityvect)
    m.sporesize[n] = mean(sporesizevect)
    m.sporesizestd[n] = std(sporesizevect)

    @printf("time: %06d, measurement: %03d, cfitness: %5.3f, numfuncs: %5.3f diversity: %5.3f, bfitness: %5.3f, sporesize: %5.3f\n",
                t, n, m.fitness[n], m.numfuncs[n],m.diversity[n],m.biofitness[n],m.sporesize[n])

end

function save(m::Measure, fname::String)
    df = DataFrame(time=m.time,
                   biofitness=m.biofitness,
                   fitness=m.fitness,
                   fitnessstd=m.fitnessstd,
                   numfuncs=m.numfuncs,
                   numfuncsstd=m.numfuncsstd,
                   diversity=m.diversity,
                   diversitystd=m.diversitystd,
                   sporesize=m.sporesize,
                   sporesizestd=m.sporesizestd)
    writetable(fname,df)
    return df
end
