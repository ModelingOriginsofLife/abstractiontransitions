#include <iostream>
#include <cstdlib>
#include <cmath>
#include <vector>

using namespace std;

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

int minidx = 0, maxidx = 100;
double miny = 0.7, maxy = 0.95;
int XBINS, YBINS = 100;

vector< vector<double> > Hist;
vector<int> Norm, Max;

int main(int argc, char **argv)
{
   FILE *f;
   int a,b,c;
   
   XBINS = maxidx - minidx + 1;
   
   int i,j;
   
   Norm.resize(XBINS,0);
   Max.resize(XBINS,0);
   
   for (i=0;i<XBINS;i++)
   {
      vector<double> lHist;
      
      lHist.resize(YBINS, 0);
      Hist.push_back(lHist);
   }   
   
   f=fopen(argv[1],"rb");
   
   while (fscanf(f,"%d %d %d\n",&a,&b,&c)!=EOF)
   {
      double v = 1.0-b/(double)c;
      
      j = YBINS * ((v-miny)/(maxy-miny));
      
      if ((j>=0)&&(j<YBINS))
      {	 
	 Norm[a]++;
	 Hist[a][j]++;
      }      
   }    
   fclose(f);
   
   for (i=0;i<XBINS;i++)
   {
      for (j=0;j<YBINS;j++)
      {
	 if (Hist[i][j] > Max[i]) Max[i] = Hist[i][j];
      }
   }
   
   f=fopen(argv[2],"wb");
   
   for (i=0;i<XBINS;i++)
   {
      for (j=0;j<YBINS;j++)
      {
	 fprintf(f,"%d %.6g %.6g\n",i, miny+(maxy-miny)*j/(double)YBINS, Max[i] ? Hist[i][j]/(double)Max[i] : 0);
      }
      fprintf(f,"\n");
   }   
   
   fclose(f);
}