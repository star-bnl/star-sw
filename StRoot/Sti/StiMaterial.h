/*
 * StiMaterial represents the physical characteristics of a material within
 * the ITTF geometry.
 */

#ifndef STI_MATERIAL_HH
#define STI_MATERIAL_HH

#include "Stiostream.h"
#include <string>
using namespace std;
#include "Sti/Base/Named.h"

class StiElossCalculator;
class StiMaterial : public Named
{    
public:
    
    // con/destructor
    StiMaterial();
    StiMaterial(const string &name,
		double z,
		double a,
		double density,
		double radLength,
		double ionization);
    virtual ~StiMaterial();
    
    /// Get the material density in grams/cubic centimeter
    double getDensity() const 		{ return _density; }
    /// Get the radiation length in g/cm^2
    double getRadLength() const 	{ return _radLength; }
    /// Get the radiation length in centimeter
    double getX0() const 		{ return _x0; }
    /// Get the effective atomic mass of the material
    double getA() const 		{ return _a;}
    /// Get the effective atomic number of the material
    double getZ() const 		{ return _z;}
    /// Get the effectice ionization potential of the material
    double getIonization() const 	{ return _ionization;}
    /// Get Z over A ratio
    double getZOverA() const			{ return _zOverA;}
    /// Get Eloss calculator 
    StiElossCalculator *getElossCalculator() const;

    void set(const string& name,
	     double z,
	     double a,
	     double density,
	     double radLength,
	     double ionization);
    
protected:
    
    /// Effective Z
    double _z;
    /// Effective A
    double _a;
    /// g/cm^3
    double _density;
    /// radiation length in g/cm^2
    double _radLength;
    /// Effective ionization (in eV)
    double _ionization;
    /// zOverA
    double _zOverA;
    /// radiation length in cm.
    double _x0;
    /// Keep Energy loss calculator
    StiElossCalculator *_eloss;
};

//Non-members--------------------------

ostream& operator<<(ostream& os, const StiMaterial& m);

#endif




