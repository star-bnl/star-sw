//StiPolygon.cxx
//M.L. Miller (Yale Software)
//06/01

//STD
#include <iostream>
#include <math.h>

//SCL
#include "StGetConfigValue.hh"

//Sti
#include "StiPolygon.h"
    
StiPolygon::StiPolygon()
{
}

StiPolygon::StiPolygon(unsigned int nsides, double phi0, double r)
    : mnsides(nsides), mphi0(phi0), mradius(r)
{
}

StiPolygon::~StiPolygon()
{
}

unsigned int StiPolygon::numberOfSides() const
{
    return mnsides;
}

double StiPolygon::phi0() const
{
    return mphi0;
}

double StiPolygon::radius() const
{
    return mradius;
}

double StiPolygon::deltaPhi() const
{
    return (mnsides>0) ? 2.*M_PI / mnsides : -999.;
}
void StiPolygon::setRadius(double val)
{
    mradius = val;
}

void StiPolygon::setNumberOfSides(unsigned int val)
{
    mnsides = val;
}

void StiPolygon::setPhi0(double val)
{
    mphi0 = val;
}

void StiPolygon::write(const char* file) const
{
    ofstream myout(file);
    myout <<*this<<endl;
    myout.close();
    return;
}

void StiPolygon::build(const char* file)
{
    StGetConfigValue(file, "mnsides", mnsides);
    StGetConfigValue(file, "mphi0", mphi0);
    StGetConfigValue(file, "mradius", mradius);
    return;
}
