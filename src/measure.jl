using DataFrames

function Measure()
    time = zeros(Int64,NUMMEAS)
    fitness = zeros(Float64,NUMMEAS)
    fitnessstd = zeros(Float64,NUMMEAS)
    numfuncs = zeros(Float64,NUMMEAS)
    numfuncsstd = zeros(Float64,NUMMEAS)

    Measure(time, fitness,fitnessstd,numfuncs,numfuncsstd)
end

function measure(pop::Population, m::Measure, t::Time, n::Int64)
    # update measurements
    #pmap(measure,pop.individuals)

    println("nummeas: $NUMMEAS , time: $t , measurement: $n")

    # store measurements in Measure
    m.time[n] = t
    fitvect = map(x->x.fitness, flatten(map(x->x.individuals,pop.individuals)))
    numfuncsvect = map(x->sum(x.genome),flatten(map(x->x.individuals,pop.individuals)))
    m.fitness[n] = mean(fitvect)
    m.fitnessstd[n] = std(fitvect)
    m.numfuncs[n] = mean(numfuncsvect)
    m.numfuncsstd[n] = std(numfuncsvect)
end

function save(m::Measure, fname::String)
    df = DataFrame(time=m.time,
                   fitness=m.fitness,
                   fitnessstd=m.fitnessstd,
                   numfuncs=m.numfuncs,
                   numfuncsstd=m.numfuncsstd)
    writetable(fname,df)
    return df
end

# function measure(me::Biofilm)
# # # update fitness, robustness, and modularity measures
# end
