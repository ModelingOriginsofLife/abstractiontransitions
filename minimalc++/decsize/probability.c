#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double Psurvive(int SS, int NF, double Pf)
{
   int *func = (int*)malloc(sizeof(int)*NF);
   int i,j,f;
   double Nfail = 0, Ntries = 0;
   int hasall;

   Nfail = 0; Ntries = 0;
      
   for (i=0;i<10000;i++)
   {
      memset(func,0,sizeof(int)*NF);
	 
      for (j=0;j<SS;j++)
      {
	 f=rand()%NF;
	 if (rand()%100000<100000*Pf)
	   func[f]=1;
      }	 
	 
      hasall=1;
      for (j=0;j<NF;j++)
        if (!func[j]) hasall=0;
	 
      if (!hasall) Nfail++;
      Ntries++;
   }
   
   free(func);
   
   return (Ntries-Nfail)/Ntries;
}

void main(int argc, char **argv)
{
   FILE *f;
   int NF = atoi(argv[1]);
   int SS = atoi(argv[2]);
   int Pf,Pf2;
   int i,j;
   
   for (Pf=1;Pf<=100;Pf++)
   {
      for (i=0;i<1000;i++)
      {
	 double Ps1,Ps2;
	 
	 Pf2 = 0;
	 
	 for (j=0;j<Pf;j++)
	 {
	    if (rand()%100000<100000*0.95) Pf2++;
	 }
	 
	 Ps1=Psurvive(SS,NF);
      }
      
      f=fopen(argv[2],"a");
      fprintf(f,"%.6g %.6g\n",Pf);
      fclose(f);
   }         
}
