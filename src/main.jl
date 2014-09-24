using StatsBase
using Datetime

function main(ARGS)

    if length(ARGS) >= 2
        configfile = ARGS[2]
    else
        configfile = "constants.jl"
        # configfile = "constants_test.jl"
    end

    # require("include.jl")
    indir = joinpath("..","input")
    outdir = joinpath("..","output")
    require(joinpath(indir,configfile))

    require("utilities.jl")
    require("types.jl")
    require("cell.jl")
    require("biofilm.jl")
    require("population.jl")
    require("measure.jl")
    #require("plot.jl")

    #-------------------------
    # setup
    #-------------------------

    if length(ARGS) >= 1
        timestamp = string(gentimestamp(short=true), "_", ARGS[1])
    else
        timestamp = gentimestamp()
    end

    simdir = joinpath(outdir,timestamp)
    run(`mkdir $simdir`)
    configpath = joinpath(indir,configfile)
    run(`cp $configpath $simdir/constants.jl`)

    #----------------------------------
    println("------------------------------")

    pop = Population()
    # meas = Measure()
    meas = MeasureAll()
    measnum = 1


    for t=1:MAXTIME

        reproduce(pop)

        if maximum( map(x->x.fitness,pop.individuals) ) == 0
           println("No viable biofilm anymore. Saving and exiting.")
           break
        end

        if (mod(t-1,MEASPERIOD)==0) | (t==MAXTIME)
            measure(pop,meas,t,measnum)
            measnum += 1
        end
    end

    #-------------------------
    # write output and plot
    #-------------------------

    # df = save(meas,joinpath(simdir,"sim.csv"))
    df = save(meas,simdir)

    println("------------------------------")
    if PLOTFLAG
        plot(simdir)
    end

    println()
    println("view data with:")
    outputpath = replace(joinpath(simdir,"sim.csv"),r"\.\./","")
    println("column -s, -t < $(outputpath)")
    #run(`cat $(joinpath(simdir,"sim.csv"))` |> `column -s, -t`)

end

function runsim(ISS,TRIAL)
  # partag = "NC$(NC)_MF$(MF)"
  partag = "ISS$(ISS)_T$(TRIAL)"
  filename = "constants_par$(partag).jl"
  f = open(joinpath("../input",filename),"w")

  s = "##################################
  ### Population size, biofilm size
  ##################################

  const NB = 100 # number of biofilms
  const NC = 100 # number of cells per biofilm

  ####################
  ### Cell definition
  ####################

  const GL = 10 # number of genes in a genome

  const DIVERSITY = \"default\" # choices are: \"random\", \"default\", or any integer
  const NG = 10  # initial number of gene per cell, used in \"default\" only.

  const SPORESIZE = \"constant\" # constant, firstPickChooses, complete
  const ISS = $(ISS) # Initial Spore Size

  const USEPROMOTERS = false
  const NP = 10 # number of types of promoter

  #######################
  ### Sampling behaviors
  #######################

  # pick members of the spore based on \"uniform\" or \"fitness\" sampling
  const GETSPORE = \"uniform\"
  # pick duplicating cell based on \"fitness\" or \"uniform\" sampling
  const GROW = \"uniform\"
  # exponent of cell fitness function
  const CFC = 1.0

  # pick reproducing biofilms based on \"fitness\" or \"uniform sampling\"
  # \"duplicate\" allow each biofilm to (try and) reproduce twice in theory.
  # CAUTION: THIS MAY BE POORLY IMPLEMENTED.
  const REPRODUCE = \"duplicate\" # population. based on fitness, uniform, duplicate
  const BFP = 1.0 # Biofilm Fitness Parameter


  ### Mutations
  const MF = 0.010 # gene mutation factor
  const PF = 0.000 # promoter mutation factor
  const HT = 0.000 # horizontal gene transfer

  ######################################################################

  ##################################
  ### Simulation related parameters
  ##################################

  const MAXTIME = 5000 # number of iterations
  const FRACMEAS = 50/MAXTIME
  const MEASPERIOD = convert(Int64,round(1/FRACMEAS))
  const NUMMEAS = convert(Int64,round(MAXTIME/MEASPERIOD) + 1)

  ##########
  ### Flags
  ##########

  const PLOTFLAG = false
  const DEBUGFLAG = false

  CONSTANTS = [NB,
               NC,
               NG,
               NP,
               GL,
               ISS,
               MF,
               PF,
               CFC,
               BFP,
               MAXTIME,
               FRACMEAS,
               MEASPERIOD,
               NUMMEAS,
               PLOTFLAG,
               DEBUGFLAG]
"

  write(f,s)
  close(f)

  # run(`cat $(joinpath("input",filename))`)
  # run(`make folder=$(partag) configfile=$(filename)`)
  main([partag, filename])
  sleep(2)
end
