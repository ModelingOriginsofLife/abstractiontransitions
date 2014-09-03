#include <iostream>
#include <cstdlib>
#include <cmath>

#include <vector>

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
		vector<int> functions;
		
		void Mutate();
		Cell();
};

Cell::Cell()
{
	functions.resize(NFUNC,1);
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
		vector<Cell> population;
		
		void initialize();
		void growFromSpore(vector<Cell> spore);
		int surviveNthGeneration(int N, int sporesize);
		int hasAllFunc();
};

class Population
{
	public:
		vector<Biofilm> population;
		int failures, attempts;
		
		void Iterate(int sporesize);
};

void Population::Iterate(int sporesize)
{
	int i,j,k;
	vector<Biofilm> newpop;
	
	for (i=0;i<population.size();i++)
	{
		for (k=0;k<2;k++)
		{
			Biofilm B;
			vector<Cell> spore;
		
			spore.clear();
		
			for (int j=0;j<sporesize;j++)
				spore.push_back(population[i].population[rand()%population[i].population.size()]);
		
			B.growFromSpore(spore);
		
			attempts++;
			if (B.hasAllFunc()) newpop.push_back(B); else failures++;
		}
	}
	
	while (newpop.size() > CARRYCAP)
	{
		newpop.erase(newpop.begin() + rand()%newpop.size());
	}
	
	population = newpop;
}

void Biofilm::initialize()
{
	population.clear();
	
	for (int i=0;i<BIOPOP;i++)
	{
		Cell C;
		
		for (int j=0;j<NFUNC;j++)
			C.functions[j] = 1;
		
		population.push_back(C);
	}
}

int Biofilm::hasAllFunc()
{
	int func[NFUNC];
	
	memset(func,0,sizeof(int)*NFUNC);
	
	for (int i=0;i<population.size();i++)
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
		spore.clear();
		for (int j=0;j<sporesize;j++)
			spore.push_back(B.population[rand()%B.population.size()]);
			
		B.growFromSpore(spore);
	
		if (!B.hasAllFunc())
			return 0;
			
		i++;
	} while (i<N);
	
	return 1;
}

void Biofilm::growFromSpore(vector<Cell> spore)
{
	int N = spore.size();
	
	population = spore;
	
	for (;N<BIOPOP;N++)
	{
		Cell newCell = spore[rand()%spore.size()];
		
		population.push_back(newCell);
	}
	
	for (int i=0;i<population.size();i++)
		population[i].Mutate();
}

int main(int argc, char **argv)
{
	Population Soup;	
	FILE *f;
	
	rseed = atoi(argv[1]);
	int Ngen = atoi(argv[2]);
	MRATE = atof(argv[3]);
	NFUNC = atoi(argv[4]);
	BIOPOP = atoi(argv[5]);
	CARRYCAP = atoi(argv[6]);
	
	char Str[512];
	
	srand(rseed);
	
	for (int ss = 1;ss<=BIOPOP;ss++)
	{
		Soup.population.clear(); Soup.failures = 0; Soup.attempts = 0;
		for (int i=0;i<CARRYCAP;i++)
		{
			Biofilm LUCA;		
			LUCA.initialize();
			Soup.population.push_back(LUCA);
		}
	
		for (int i=0;i<Ngen;i++)
			Soup.Iterate(ss);
			
		f=fopen(argv[7],"a");
		fprintf(f,"%d %d %d\n",ss,Soup.failures, Soup.attempts);
		fclose(f);
	}
}
