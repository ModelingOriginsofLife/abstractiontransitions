using Base.Test
#using Debug

# testdir
require("input/constants_test.jl")

# srcdir
require("src/types.jl")
require("src/cell.jl")

function testcells()
    newcell = Cell()
    @test length(newcell.genome) == GL
    @test length(newcell.promoter) == GL
end

testcells()
