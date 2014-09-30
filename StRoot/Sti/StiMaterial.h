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
		double X0);
    virtual ~StiMaterial();
    StiMaterial(const string &name,
		double z,
		double a,
		double density,
		double X0xDensity,
		double ionization);
    
    /// Get the material density in grams/cubic centimeter
    double getDensity() const 		{ return _density; }
    /// Get the radiation length in centimeter
    double getX0() const 		{ return _x0; }
    /// Get the effective atomic mass of the material
    double getA() const 		{ return _a;}
    /// Get the effective atomic number of the material
    double getZ() const 		{ return _z;}
    /// Get Z over A ratio
    double getZOverA() const		{ return _zOverA;}
    /// Dummy method
    double getIonization() const	{ return -999;}

    /// Get Eloss calculator 
    StiElossCalculator *getElossCalculator() const;

    void set(const string& name,
	     double z,
	     double a,
	     double density,
	     double radLength,// X0*density (for compatibility)
	     double ionization);
    void set(const string& name,
	     double z,
	     double a,
	     double density,
	     double X0);// X0
    
protected:
    
    /// Effective Z
    double _z;
    /// Effective A
    double _a;
    /// g/cm^3
    double _density;
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




