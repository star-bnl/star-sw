/*
 * StiMaterial represents the physical characteristics of a material within
 * the ITTF geometry.
 */

#ifndef STI_MATERIAL_HH
#define STI_MATERIAL_HH

#include <iostream>
#include <string>
using namespace std;
//using std::string;

class StiMaterial
{    
public:
    
    // con/destructor
    StiMaterial();
    virtual ~StiMaterial();
    
    // accessors
    double getDensity() const { return dDensity; }
    double getRadLength() const { return dRadLength; }
    const string& getName() const { return szName; }
    
    // mutators
    void setDensity(double val){ dDensity = val; }
    void setRadLength(double val){ dRadLength = val; }
    void setName(const string& val){ szName=val; }
    
protected:
    
    double dDensity;   // g/cm^3
    double dRadLength; // cm
    string szName;
    
};

//Non-members--------------------------

ostream& operator<<(ostream& os, const StiMaterial& m);

#endif




