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

    switchon(idx, newcell.genome)
    @test(newcell.genome[1] == true)
    @test(newcell.genome[3] == true)

    switchoff(idx, newcell.genome)
    @test(newcell.genome[1] == false)
    @test(newcell.genome[3] == false)

    idx = mutating(10, 0.)
    @test(length(idx) == 0)

    idx = mutating(10, 1.)
    @test(length(idx) == 10)

end

function testpromoters()
    cell = Cell()
    cell.genome = trues(GL)
    cell.promoter = [1:GL]
    cell.expressed = getexpressed(cell.genome, cell.promoter)
    @test(cell.expressed == trues(GL))

    cell.promoter =[1:GL]*0 + 1 
    cell.expressed = getexpressed(cell.genome, cell.promoter)
    @test(cell.expressed == falses(GL))
end

testgetfitness()
testmutate()
testpromoters()