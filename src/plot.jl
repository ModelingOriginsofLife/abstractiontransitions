function plot2(simdir)

    outputs = filter(x->contains(x,"csv"), readdir(simdir))
    for file in outputs
        fname = joinpath(simdir, file)
        println(fname)
        run(`python plotdata.py -f $fname`)
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
