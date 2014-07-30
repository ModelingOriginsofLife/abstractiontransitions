using Base.Test
#using Debug

# testdir
require("input/constants_test.jl")

# srcdir
require("src/types.jl")
require("src/cell.jl")
require("src/biofilm.jl")

function testcells()
    biofilm = Biofilm(1)
    grow(biofilm)
    @test biofilm.individuals[2].genome == biofilm.individuals[3].genome
    @test biofilm.individuals[2].promoter == biofilm.individuals[3].promoter
end

testcells()
