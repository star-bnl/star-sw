
#include "PythiaParticle.h"

#include <iostream>
#include <sstream>

using namespace std;

ClassImp(PythiaParticle)


PythiaParticle::PythiaParticle( const std::string& line1)
{
	//create a stringstream to input certain data line by line
	std::stringstream ss1;
	ss1 << line1;
	ss1 >> index >> KS >> id >> mother >> daughter1 >> daughter2
	>> px >> py >> pz >> E >> m >> x >> y >> z;
}


void PythiaParticle::print()
{
	cout << index << " " << m << std::endl;
}
