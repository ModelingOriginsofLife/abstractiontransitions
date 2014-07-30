function plot(simdir)
    plotspdf = joinpath(simdir,"plots.pdf")
    plotxvar = "time"
    plotyvar = ["fitness", "numfuncs","diversity"]
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
    simdir=joinpath("..","output",ARGS[1])
    plot(simdir)
end
