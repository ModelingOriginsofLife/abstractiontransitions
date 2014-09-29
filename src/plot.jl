function plot2(simdir)

    include(joinpath(simdir,"constants.jl"))
    outputs = filter(x->contains(x,"csv"), readdir(simdir))
    for file in outputs
        fname = joinpath(simdir, file)
        println(fname)

        if isdefined(:CONSTANTS)
            run(`python plotdata.py -f $fname -p $CONSTANTS`)
        else
            run(`python plotdata.py -f $fname`)
        end
    end

    plotspdf = joinpath(simdir,"$(split(simdir,'/')[3]).pdf")
    plotyvar = ["biofitness",
                "biofitness_hist",
                "fitness",
                "fitness_hist",
                "numfuncs_hist",
                "numfuncs",
                "expressed",
                "expressed_hist",
                "diversity",
                "diversity_hist",
                "sporesize",
                "sporesize_hist"]

    plotspdfs = map(x->joinpath(simdir,string(x,".pdf")),plotyvar)
    run(`gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=$plotspdf
            -dBATCH $plotspdfs`)
    println("\nview plots with:")
    outputpath = replace(plotspdf,r"\.\./","")
    println("evince $outputpath")
end


function plot(simdir)
    plotspdf = joinpath(simdir,"plots.pdf")
    plotxvar = "time"
    plotyvar = ["biofitness", "fitness", "numfuncs", "expressed", "diversity","sporesize"]
    plotspdfs = map(x->joinpath(simdir,string(x,".pdf")),plotyvar)
    run(`python plotdata.py -d $simdir -x $plotxvar -y $plotyvar`)
    run(`gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=$plotspdf
            -dBATCH $plotspdfs`)
    println("\nview plots with:")
    outputpath = replace(plotspdf,r"\.\./","")
    println("evince $outputpath")
    # spawn(`evince $plotspdf`)
end

if length(ARGS)>0
    simdir=joinpath("..",ARGS[1])
    plot2(simdir)
end

function extractsurvival(pathtorepo,numss,numtrials,datestrings)

    # pathtorepo = "$HOME/mnt/cluster/abstractiontransitions"
    # numss = 100
    # numtrials = 15
    # datestrings = ["2014925","2014926","2014927"]

    rv = (String)[]
    push!(rv,"survival\tsporesize")

    for i = 1:numss, j=1:numtrials
        for datestring in datestrings
            datfile = joinpath(pathtorepo,
                      "output",
                      "$(datestring)_ISS$(i)_T$(j)",
                      "survival.csv")
            if isfile(datfile)
                sssfile = joinpath(pathtorepo,
                      "output",
                      "$(datestring)_ISS$(i)_T$(j)",
                      "sporesize.csv")
                sss = readdlm(sssfile)
                column = readdlm(datfile)
                line = "$(column[end,1])\t$(sss[1,1])"
                push!(rv,line)
            end
        end
    end

    rv = join(rv,"\n")
    f = open("../output/survivalsporesize.tsv","w")
    write(f,rv)
    close(f)

    dat,head = readdlm("../output/survivalsporesize.tsv",header=true)
    sss = zeros(numss,numtrials)
    for i = 1:numss
        ind = find(x->x==i,dat[:,2])
        if length(ind) > 0
            if length(ind) <= numtrials
                sss[i,1:length(ind)]=dat[ind,1]
            else
                sss[i,:] = dat[ind[1:numtrials],1]
            end
        end
    end
    f = open("../output/survivalsporesize.csv","w")
    writedlm(f,sss)
    close(f)

    return rv

end
