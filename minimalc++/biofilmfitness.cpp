#include <iostream>
#include <cstdlib>
#include <cmath>
#include <vector>
#include <random>
#include <mpi.h>

using namespace std;

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

mt19937 generator;

int BIOPOP=100;
double MRATE=1e-2;
double HGTRATE=0;
int NFUNC=10;
int CARRYCAP=100;
int rseed = 0;

int prand(double P)
{	
	bernoulli_distribution distribution(P);
	return distribution(generator);	
}

int irand(int min, int max)
{
	uniform_int_distribution<int> distribution(min, max-1);
	
	return distribution(generator);
}

class Cell
{
	public:
		char *functions;
		
		void HGT(Cell *Other);
		void Mutate();
		void alloc();
		
		Cell& operator=(Cell &b);
};

Cell *spore;

Cell& Cell::operator=(Cell &b)
{
	memcpy(functions, b.functions, sizeof(char)*NFUNC);
	
	return *this;
}

void Cell::alloc()
{
	functions = (char*)malloc(sizeof(char)*NFUNC);
}

void Cell::Mutate()
{
	for (int i=0;i<NFUNC;i++)
	{
		if (prand(MRATE))
			functions[i] = 0;
	}
}

void Cell::HGT(Cell *Other)
{
	int idx = irand(0, NFUNC);
	functions[idx] = Other->functions[idx];
}

class Biofilm
{
	public:
		Cell *population;
		
		void initialize();
		void growFromSpore(Biofilm *parent, int sporesize);
		int surviveNthGeneration(int N, int sporesize);
		int hasAllFunc();
		void alloc();

		Biofilm& operator=(Biofilm &b);
};

Biofilm& Biofilm::operator=(Biofilm &b)
{
	for (int i=0;i<BIOPOP;i++)
	{
		population[i] = b.population[i];
	}

	return *this;
}

void Biofilm::alloc()
{
	population = (Cell*)malloc(sizeof(Cell)*BIOPOP);
	for (int i=0;i<BIOPOP;i++)
	{
		population[i].alloc();
	}
}

class Population
{
	public:
		Biofilm *population;
		Biofilm *newpop;
		int pCount;
		
		int failures, attempts;
		
		void Iterate(int sporesize);
		void alloc();
		
		Population& operator=(Population &b);
};

Population& Population::operator=(Population &b)
{
	for (int i=0;i<CARRYCAP;i++)
	{
		population[i] = b.population[i];
	}

	return *this;
}

void Population::alloc()
{	
	population = (Biofilm*)malloc(sizeof(Biofilm)*CARRYCAP);
	newpop = (Biofilm*)malloc(sizeof(Biofilm)*CARRYCAP*2);
	
	for (int i=0;i<CARRYCAP;i++)
		population[i].alloc();

	for (int i=0;i<2*CARRYCAP;i++)
		newpop[i].alloc();
}

int *randomOrder;

void getRandomOrder(int l)
{
	for (int i=0;i<l;i++)
		randomOrder[i] = i;
		
	for (int i=0;i<l-1;i++)
	{
		int j = irand(0,l-i)+i;
		int buf = randomOrder[i];
		randomOrder[i] = randomOrder[j];
		randomOrder[j] = buf;
	}
}

void Population::Iterate(int sporesize)
{
	int i,j,k;	
	int npCount = 0;
	
	getRandomOrder(pCount);
	for (i=0;(i<pCount)&&(npCount<CARRYCAP);i++)
	{
		j = randomOrder[i];
		for (k=0;k<2;k++)
		{
			if (npCount<CARRYCAP)
			{
				newpop[npCount].growFromSpore(&population[j], sporesize);
		
				attempts++;
				if (newpop[npCount].hasAllFunc()) { npCount++; } else failures++;
			}
		}
	}
	
/*	while (npCount > CARRYCAP)
	{
		i = rand()%npCount;
		
		newpop[i] = newpop[npCount-1];
		npCount--;
	}
	*/
	
	for (i=0;i<npCount;i++)
		population[i] = newpop[i];
		
	pCount = npCount;
}

void Biofilm::initialize()
{
	for (int i=0;i<BIOPOP;i++)
	{
		for (int j=0;j<NFUNC;j++)
			population[i].functions[j] = 1;
	}
}

int Biofilm::hasAllFunc()
{
	int func[NFUNC];
	
	memset(func,0,sizeof(int)*NFUNC);
	
	for (int i=0;i<BIOPOP;i++)
	{
		for (int j=0;j<NFUNC;j++)
		{
			if (population[i].functions[j]) func[j] = 1;
		}
	}
	
	for (int i=0;i<NFUNC;i++)
		if (!func[i]) return 0;
	
	return 1;
}

int Biofilm::surviveNthGeneration(int N, int sporesize)
{
	vector<Cell> spore;
	int i=0;
	int total = 0;
			
	Biofilm B;
	
	B.population = population;
	
	do
	{
		B.growFromSpore(&B,sporesize);
	
		if (!B.hasAllFunc())
			return 0;
			
		i++;
	} while (i<N);
	
	return 1;
}

void Biofilm::growFromSpore(Biofilm *parent, int sporesize)
{
	int N = sporesize;
	
	for (int i=0;i<sporesize;i++)
	{
		population[i] = parent->population[irand(0,BIOPOP)];		
	}
	
	for (;N<BIOPOP;N++)
	{
		population[N] = population[irand(0,sporesize)];
	}
	
	for (int i=0;i<BIOPOP;i++)
		population[i].Mutate();

	for (int i=0;i<BIOPOP;i++)
	{
		if (prand(HGTRATE))
			population[i].HGT(&population[irand(0,BIOPOP)]);
	}
}

int main(int argc, char **argv)
{
	FILE *f;
	int thread_id;
	
	MPI_Init(NULL,NULL);
	MPI_Comm_rank(MPI_COMM_WORLD,  &thread_id);
	
	rseed = atoi(argv[1]) + thread_id;
	int Ngen = atoi(argv[2]);
	MRATE = atof(argv[3]);
	NFUNC = atoi(argv[4]);
	BIOPOP = atoi(argv[5]);
	CARRYCAP = atoi(argv[6]);
	HGTRATE = atof(argv[7]);
	
	randomOrder = (int*)malloc(sizeof(int) * CARRYCAP);
		
	char Str[512];
	
	generator.seed(rseed);
		
	Population Soup;
	
	Soup.alloc();
	
	for (int ss = 1;ss<=BIOPOP;ss++)
	{
		Soup.failures = 0; Soup.attempts = 0;
		for (int i=0;i<CARRYCAP;i++)
		{
			Soup.population[i].initialize();
		}
		Soup.pCount = CARRYCAP;
		
		for (int i=0;i<Ngen;i++)
			Soup.Iterate(ss);
			
		sprintf(Str,"%s_%d.txt",argv[8],rseed);
		f=fopen(Str,"a");
		fprintf(f,"%d %d %d %.6g %.6g %d %d %d\n",ss,Soup.failures, Soup.attempts, MRATE, HGTRATE, BIOPOP, CARRYCAP, NFUNC);
		fclose(f);
	}
	
	MPI_Finalize();
}
