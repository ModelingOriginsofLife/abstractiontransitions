using Base.Test
#using Debug

# testdir
require("test/constants.jl")

# srcdir
require("src/types.jl")
require("src/cell.jl")

function testcells()
    newcell = Cell()
    @test length(newcell.genome) == GL
    @test length(newcell.promoter) == GL
end

testcells()
