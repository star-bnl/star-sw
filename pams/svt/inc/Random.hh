#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#ifndef _RANDOM_
#define _RANDOM_

class Random
	{
		protected:	

		unsigned int seed;

		public:

		Random();
		Random(unsigned int newSeed);

		void setSeed(unsigned int newSeed);
		unsigned int getSeed();

		double getValue();

	};

#endif

