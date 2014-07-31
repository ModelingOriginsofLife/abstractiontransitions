using Base.Test
#using Debug

# testdir
require("input/constants_test.jl")

# srcdir
require("src/types.jl")
require("src/cell.jl")

newcell = Cell()

function testgetfitness()
    g0 = falses(GL)
    @test_approx_eq(getfitness(g0), 1.0)

    g1 = trues(GL)
    @test_approx_eq(getfitness(g1), 0.0)

    newcell.genome = g1
    newcell.promoter = (Integer)[1:GL]
    @test_approx_eq(getfitness(newcell), 0.0)
end

function testmutate()
    oldcell = deepcopy(newcell)
    idx = [1 3]
    switchbits(idx, newcell.genome)
    @test(newcell.genome[1] == !oldcell.genome[1])
    @test(newcell.genome[3] == !oldcell.genome[3])

    idx = mutating(10, 0.)
    @test(length(idx) == 0)

    idx = mutating(10, 1.)
    @test(length(idx) == 10)
end

testgetfitness()
testmutate()
