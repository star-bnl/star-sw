//StiDetPolygon.cxx
//M.L. Miller (Yale Software)
//06/01

#include <iostream>
#include <algorithm>
#include <math.h>
#include <cmath>

#include "StiDetector.h"
#include "StiDetPolygon.h"

ClassImp(StiDetPolygon)
    
StiDetPolygon::StiDetPolygon()
{
}

StiDetPolygon::StiDetPolygon(unsigned int nsides, double phi0, double r)
    : StiPolygon(nsides, phi0, r)
{
}

StiDetPolygon::~StiDetPolygon()
{
}

unsigned int StiDetPolygon::numberOfDetectors() const
{
    return msidemap.size();
}

bool StiDetPolygon::isValid() const
{
    return (numberOfDetectors() == numberOfSides() );
}

void StiDetPolygon::push_back(StiDetector* layer)
{
    double phi = layer->getCenterRefAngle();
    unsigned int theside = side(phi);
    //cout <<(*layer)<<" With Phi "<<phi<<" At Side "<<theside<<endl;
    det_polygon_map::iterator where = msidemap.find(theside);
    if (where!=msidemap.end() ) {
	cout <<"StiDetPolygon::push_back(StiDetector*) !!!! Error: key already taken"<<endl;
	//cout <<"Calling Side in Debug Mode"<<endl;
	//theside = side(phi, true);
    }
    msidemap.insert( det_polygon_map_ValType( theside,layer ) );
    return;
}

void StiDetPolygon::print() const
{
    for (det_polygon_map::const_iterator it=msidemap.begin(); it!=msidemap.end(); ++it) {
	cout <<(*it).first<<"\t"<<*( (*it).second)<<endl;
    }
    return;
}

unsigned int StiDetPolygon::side(double angle, bool debug) const
{
    bool go = true;
    while (go) {
	if (angle>2.*M_PI) { angle-=2.*M_PI; }
	else { go=false;}
    }
    if (deltaPhi()==0.) {
	cout <<"StiDetPolygon::side(double) !! ERROR: deltaPhi()==0."<<endl;
	return 999;
    }
    double side_d = (angle-phi0())/deltaPhi();
    unsigned int side_ui = static_cast<unsigned int> (floor(side_d + .5) );
    if (debug) {
	cout <<angle<<"\t"<<side_d<<"\t"<<side_ui<<"\t"<<static_cast<double>(side_ui)<<endl;	
    }
    return side_ui;
}
