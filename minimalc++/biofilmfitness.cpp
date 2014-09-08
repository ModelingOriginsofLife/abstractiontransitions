#include <iostream>
#include <cstdlib>
#include <cmath>
#include <vector>

#include <mpi.h>

using namespace std;

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

int BIOPOP=100;
double MRATE=1e-2;
int NFUNC=10;
int CARRYCAP=100;
int rseed = 0;

int prand(double P)
{
	if (rand()%1000000<1000000*P) return 1;
	return 0;
}

class Cell
{
	public:
		char *functions;
		
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

void Population::Iterate(int sporesize)
{
	int i,j,k;	
	int npCount = 0;
	
	for (i=0;i<pCount;i++)
	{
		for (k=0;k<2;k++)
		{
			newpop[npCount].growFromSpore(&population[i], sporesize);
		
			attempts++;
			if (newpop[npCount].hasAllFunc()) { npCount++; } else failures++;
		}
	}
	
	while (npCount > CARRYCAP)
	{
		i = rand()%npCount;
		
		newpop[i] = newpop[npCount-1];
		npCount--;
	}
	
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
		spore[i] = parent->population[rand()%BIOPOP];
		population[i] = spore[i];
	}
	
	for (;N<BIOPOP;N++)
	{
		population[N] = spore[rand()%sporesize];
	}
	
	for (int i=0;i<BIOPOP;i++)
		population[i].Mutate();
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
	
	char Str[512];
	
	srand(rseed);
	
	spore = (Cell*)malloc(BIOPOP*sizeof(Cell));
	
	for (int i=0;i<BIOPOP;i++)
	{
		spore[i].alloc();
	}
	
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
			
		printf("Ping!\n");
		sprintf(Str,"%s_%d.txt",argv[7],thread_id);
		f=fopen(Str,"a");
		fprintf(f,"%d %d %d\n",ss,Soup.failures, Soup.attempts);
		fclose(f);
	}
	
	MPI_Finalize();
}
