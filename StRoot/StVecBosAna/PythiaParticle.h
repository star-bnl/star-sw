#ifndef PythiaParticle_h
#define PythiaParticle_h

#include <TObject.h>


class PythiaParticle : public TObject
{
public:

   // Variables for the particle
	int    index;
	int    id;
	int    KS;
	int    mother;
	int    daughter1;
	int    daughter2;
	double px;
	double py;
	double pz;
	double E;
	double m;
	double x;
	double y;
	double z;

   // Derived variables like pt, theta, phi, rapidity...
	double pt;
	double p;
	double theta;
	double phi;
	double rapidity;
	double eta;

   PythiaParticle( const std::string& line1 = "" );

   void print();

	ClassDef(PythiaParticle, 1)
};

#endif
