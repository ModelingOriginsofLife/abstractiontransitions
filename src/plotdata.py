#!/usr/bin/env python

"""
plotdata.py

Read data from csv file
into pandas dataframe, filter, and
plot data

input: DATA-FILENAME.csv
output: OUTPUT-FILENAME.pdf
"""

import os
import sys
import argparse
import subprocess
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rc
import pandas as pd
#import d3py

def readdata(filename,printlevel=0):
    """
    input: filehandle
    output: dataframe object containing data from filename
    """

    fhandle = open(filename,'r')
    datframe = pd.read_csv(fhandle,error_bad_lines=False)
    fhandle.close()

    if printlevel:
        print datframe
        print datframe['first_name']
        print datframe.describe()
        print 'number of columns: ' + str(datframe.columns.size)
        print datframe.columns.tolist()

    return datframe

def main(argv):
    # Read the arguments
    parser = argparse.ArgumentParser(description="Plot data from csv file")
    #parser.add_argument('-d', '--data-filename', type=argparse.FileType('r'),
    #        help="the .csv file to use as the source", required=True)
    parser.add_argument('-d', '--data-directory', type=str,
            help="the directory for reading and writing data", required=True)
    #parser.add_argument('-o', '--output-filename', type=str,
    #        help="the .pdf output file", required=True)
    parser.add_argument('-y', '--y-list', type=str, nargs='+', help="list of names of y-axis data", required=True)
    parser.add_argument('-x', '--x-data', type=str, default="time",
            help="the name of the x-axis data")
    options = parser.parse_args(argv[1:])
    #print options.data_filename
    #print options.output_filename
    #dfname = str(argv[0]) #"fourgraphs.dat"
    #ffname = str(argv[1]) # "fourgraphsvolrat.pdf"
    #ypar = str(argv[2])
    #delimiter = '  '

    df = readdata(os.path.join(options.data_directory,"sim.csv"))

    rc('text', usetex=True)
    rc('font', family='serif')

    for y_data in options.y_list:
        fig1=plt.figure(num=1,figsize=(12,9),facecolor='w')
        ax1f1 = fig1.add_subplot(111)
        ax1f1.set_ylabel(y_data,
                         fontsize=30,labelpad=20,fontweight='normal')
        ax1f1.set_xlabel('generation',fontsize=30,labelpad=10)
        ax1f1.set_xlim((0, max(df[options.x_data])+1))
        ax1f1.set_ylim((min(df[y_data]) - .1*min(df[y_data]),
                        max(df[y_data]) + .1*max(df[y_data])))
        #ax1f1.set_xticklabels(('','1','2','3','4','5','6','7',''))
        #ax1f1.set_xticklabels(('','','','','','','','',''))
        #ax1f1.set_yticklabels(('','.2','','.6','','1'))
        ax1f1.tick_params(axis='both', which='major', labelsize=30)
        plt.grid(True)
        f1p1 = plt.plot(df[options.x_data],df[y_data],
                        linestyle='-', linewidth=5, color='k',
                        marker='o',markersize=12,
                        mec='k',mfc='r')
        if (y_data+"std" in list(df.columns)):
            plt.errorbar(df[options.x_data],df[y_data],
                        yerr=df[y_data+"std"],
                        linestyle="None", marker="None",
                        color="k")
            ax1f1.set_ylim((min(df[y_data])-max(df[y_data+"std"]),
                            max(df[y_data])+max(df[y_data+"std"])))
        output_filename = os.path.join(options.data_directory,y_data+".pdf")
        plt.savefig(output_filename,bbox_inches='tight',
            facecolor=fig1.get_facecolor(), edgecolor='none')
        plt.close()

    #vnc = subprocess.check_output(["evince",options.output_filename])

if __name__ == "__main__":
    main(sys.argv)
