//StiDetPolygon.cxx
//M.L. Miller (Yale Software)
//06/01

#include <iostream>
#include <algorithm>
#include <math.h>
#include <cmath>

#include "StiDetector.h"
#include "StiDetPolygon.h"

//ClassImp(StiDetPolygon)
    
StiDetPolygon::StiDetPolygon()
{
}

StiDetPolygon::StiDetPolygon(unsigned int nsides, double phi0, double r)
    : StiPolygon(nsides, phi0, r)
{
}

StiDetPolygon::~StiDetPolygon()
{
    clearAndDestroy();
}

unsigned int StiDetPolygon::numberOfDetectors() const
{
    return msidemap.size();
}

bool StiDetPolygon::isValid() const
{
    return (numberOfDetectors() == numberOfSides() );
}

void StiDetPolygon::clear()
{
    msidemap.clear();
}

void StiDetPolygon::clearAndDestroy()
{
    for (det_polygon_map::iterator it=msidemap.begin(); it!=msidemap.end(); ++it) {
	delete (*it).second;
	(*it).second = 0;
    }
    return;
}

void StiDetPolygon::reset()
{
    mcurrent = msidemap.begin();
}

//Set to iterator to side closest to this angle
void StiDetPolygon::setToAngle(double angle)
{
    det_polygon_map::iterator where = msidemap.find( side(angle) );
    if (where == msidemap.end() ) {
	cout <<"StiDetPolygon::setToAngle(double) !!! ERROR: side for angle: "<<angle<<" not found"<<endl;
	return;
    }
    else {
	mcurrent = where;
    }
}

StiDetector* StiDetPolygon::detector(unsigned int side) const
{
    det_polygon_map::const_iterator where = msidemap.find(side);
    return (where!=msidemap.end()) ? (*where).second : 0;
}

//iterate through side container
void StiDetPolygon::operator++()
{
    //this first if is a redundant check (in principle, you never go out of bounds, but we'll stick it in to be sure
    if (mcurrent==msidemap.end()) {
	mcurrent=msidemap.begin(); //Wrap around 2pi
    }
    else {
	++mcurrent;
	if (mcurrent == msidemap.end() ) {
	    mcurrent = msidemap.begin(); //Wrap around 2pi
	}
    }
    return;
}

void StiDetPolygon::operator--()
{
    if (mcurrent==msidemap.begin()) {
	mcurrent = --msidemap.end(); //Wrap around 2pi
    }
    
    else {
	--mcurrent;
    }
    return;
}

StiDetector* StiDetPolygon::operator*() const
{
    return (mcurrent != msidemap.end()) ? (*mcurrent).second : 0;
}



StiDetector* StiDetPolygon::detector(double angle) const
{
    return ( detector( side(angle) ) );
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
