/*
 * StiMaterial represents the physical characteristics of a material within
 * the ITTF geometry.
 */

#ifndef STI_MATERIAL_HH
#define STI_MATERIAL_HH

#include <iostream>
#include <string>
using namespace std;
#include "Named.h"

class StiMaterial : public Named
{    
public:
    
    // con/destructor
    StiMaterial();
    virtual ~StiMaterial();
    
    /// Get the material density in grams/cubic centimeter
    double getDensity() const { return _density; }
    /// Get the radiation length in centimeter
    double getRadLength() const { return _radLength; }
    /// Get the effective atomic mass of the material
    double getA() const { return _a;}
    /// Get the effective atomic number of the material
    double getZ() const { return _z;}
    /// Get the effectice ionization potential of the material
    double getIonization() const { return _ionization;}
    
    /// Set all material attributes.
    void set(const string& name,
	     double density,
	     double radLength,
	     double a,
	     double z,
	     double ionization)
      {
	_name = name;
	_density = density;
	_radLength = radLength;
	_a = a;
	_z = z;
	_ionization = ionization;
      }
    /// Set the density of the material in units of grams/cubic centimeter
    void setDensity(double val){ _density = val; }
    /// Set the radiation length of the material in centimeters
    void setRadLength(double val){ _radLength = val; }
    /// Set the effective atomic numbere of the material
    void setA(double val) { _a = val; }
    /// Set the effective atomic number of the material
    void setZ(double val) { _z = val; }
    /// Set the ionization potential of the material in eV
    void setIonization(double val) { _ionization = val; }

protected:
    
    /// g/cm^3
    double _density;
    /// cm
    double _radLength;
    /// Effective Z
    double _z;
    /// Effective A
    double _a;
    /// Effective ionization (in eV)
    double _ionization;
};

//Non-members--------------------------

ostream& operator<<(ostream& os, const StiMaterial& m);

#endif




