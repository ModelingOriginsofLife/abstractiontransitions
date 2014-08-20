using DataFrames

function Measure()
    time = zeros(Int64,NUMMEAS)
    biofitness = zeros(Float64,NUMMEAS)
    biofitnessstd = zeros(Float64,NUMMEAS)
    fitness = zeros(Float64,NUMMEAS)
    fitnessstd = zeros(Float64,NUMMEAS)
    numfuncs = zeros(Float64,NUMMEAS)
    numfuncsstd = zeros(Float64,NUMMEAS)
    diversity = zeros(Float64,NUMMEAS)
    diversitystd = zeros(Float64,NUMMEAS)
    sporesize = zeros(Float64,NUMMEAS)
    sporesizestd = zeros(Float64,NUMMEAS)
    expressed = zeros(Float64,NUMMEAS)
    expressedstd = zeros(Float64,NUMMEAS)

    Measure(time,biofitness,biofitnessstd,fitness,fitnessstd,numfuncs,numfuncsstd,diversity,diversitystd,
                 sporesize,sporesizestd,expressed,expressedstd)
end

function MeasureAll()
    time = zeros(Int64,NUMMEAS)
    biofitness = zeros(Float64,NUMMEAS,NB)
    fitness = zeros(Float64,NUMMEAS,NB*NC)
    numfuncs = zeros(Float64,NUMMEAS,NB*NC)
    diversity = zeros(Float64,NUMMEAS,NB)
    sporesize = zeros(Float64,NUMMEAS,NB)
    expressed = zeros(Float64,NUMMEAS,NB*NC)

    MeasureAll(time,biofitness,fitness,numfuncs,diversity,
                 sporesize,expressed)
end

function measure(pop::Population, m::Measure, t::Time, n::Int64)
    # Array{Array{Cell,1},1}
    cellsinbiofilms = map(x->x.individuals,pop.individuals)

    # Array{Array{BitArray{1},1},1}
    genomesincells = map(x->map(y->y.genome,x),cellsinbiofilms)

    # store measurements in Measure
    m.time[n] = t
    fitvect = map(x->x.fitness, flatten(cellsinbiofilms))
    biofitvect = map(x->x.fitness, pop.individuals)

    numfuncsvect = map(x->sum(x.genome),flatten(cellsinbiofilms))
    expressedvect = map(x->sum(x.expressed),flatten(cellsinbiofilms))
    diversityvect = map(x->length(unique(x)),genomesincells)
    sporesizevect = map(x->x.sporesize,pop.individuals)

    m.biofitness[n] = mean(biofitvect)
    m.biofitnessstd[n] = std(biofitvect)
    m.fitness[n] = mean(fitvect)
    m.fitnessstd[n] = std(fitvect)
    m.numfuncs[n] = mean(numfuncsvect)
    m.numfuncsstd[n] = std(numfuncsvect)
    m.diversity[n] = mean(diversityvect)
    m.diversitystd[n] = std(diversityvect)
    m.sporesize[n] = mean(sporesizevect)
    m.sporesizestd[n] = std(sporesizevect)
    m.expressed[n] = mean(expressedvect)
    m.expressedstd[n] = std(expressedvect)

    @printf("time: %06d, measurement: %03d, cfitness: %5.3f, numfuncs: %5.3f, expressed: %5.3f, diversity: %5.3f, bfitness: %5.3f, sporesize: %5.3f\n",
                t, n, m.fitness[n], m.numfuncs[n], m.expressed[n], m.diversity[n],m.biofitness[n],m.sporesize[n])

end

function measure(pop::Population, m::MeasureAll, t::Time, n::Int64)
    # Array{Array{Cell,1},1}
    cellsinbiofilms = map(x->x.individuals,pop.individuals)

    # Array{Array{BitArray{1},1},1}
    genomesincells = map(x->map(y->y.genome,x),cellsinbiofilms)

    # store measurements in Measure
    m.time[n] = t
    fitvect = map(x->x.fitness, flatten(cellsinbiofilms))
    biofitvect = map(x->x.fitness, pop.individuals)

    numfuncsvect = map(x->sum(x.genome),flatten(cellsinbiofilms))
    expressedvect = map(x->sum(x.expressed),flatten(cellsinbiofilms))
    diversityvect = map(x->length(unique(x)),genomesincells)
    sporesizevect = map(x->x.sporesize,pop.individuals)

    m.biofitness[n,:] = biofitvect
    m.fitness[n,:] = fitvect
    m.numfuncs[n,:] = numfuncsvect
    m.diversity[n,:] = diversityvect
    m.sporesize[n,:] = sporesizevect
    m.expressed[n,:] = expressedvect

    @printf("time: %06d, measurement: %03d, cfitness: %5.3f, numfuncs: %5.3f, expressed: %5.3f, diversity: %5.3f, bfitness: %5.3f, sporesize: %5.3f\n",
                t,
                n,
                mean(m.fitness[n,:]),
                mean(m.numfuncs[n,:]),
                mean(m.expressed[n,:]),
                mean(m.diversity[n,:]),
                mean(m.biofitness[n,:]),
                mean(m.sporesize[n,:]))

end

function save(m::Measure, fname::String)
    df = DataFrame(time=m.time,
                   biofitness=m.biofitness,
                   biofitnessstd=m.biofitnessstd,
                   fitness=m.fitness,
                   fitnessstd=m.fitnessstd,
                   numfuncs=m.numfuncs,
                   numfuncsstd=m.numfuncsstd,
                   diversity=m.diversity,
                   diversitystd=m.diversitystd,
                   sporesize=m.sporesize,
                   sporesizestd=m.sporesizestd,
                   expressed=m.expressed,
                   expressedstd=m.expressedstd)
    writetable(fname,df)
    return df
end

function save(m::MeasureAll, pathroot::String)

    for field in MeasureAll.names
      if !(string(field)=="time")
        writedlm(joinpath(pathroot,string(field,".csv")),getfield(m,field))
      end
    end
    return 0

end
