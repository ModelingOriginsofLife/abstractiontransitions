configfile = "constants.jl"
# configfile = "constants_test.jl"
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
