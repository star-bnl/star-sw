#include "Random.hh"

Random::Random()
	{
		seed = (unsigned)time( NULL );
		srand(seed);
	}

Random::Random(unsigned int newSeed)
	{
		seed = newSeed;
		srand(seed);
	}

void Random::setSeed(unsigned int newSeed)
	{
		seed = newSeed;
		srand(seed);
	}

unsigned int Random::getSeed()
	{
		return seed;
	}

double Random::getValue()
	{
		return (double) rand()/RAND_MAX;
	}
