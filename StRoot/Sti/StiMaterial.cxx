//StiMaterials.h
//M.L. Miller (Yale Software)
//04/01

//#include "StGetConfigValue.hh"
#include <string.h>
#include <stdexcept>
#include "Sti/StiMaterial.h"

StiMaterial::StiMaterial(){
} // StiMaterial()

StiMaterial::StiMaterial(const string &name,
			 double z,
			 double a,
			 double density,
			 double radLength,
			 double ionization)
{
  set(name,z,a,density,radLength,ionization);
}

StiMaterial::~StiMaterial(){
} // ~StiMaterial()

/*! Set all material attributes.
 \param name name given to the material
 \param effective mass number of the material
 \param effective atomic mass of the material
 \param density of the material in g/cm^3
 \param radiation length in g/cm^2 -> _x0 as calculated is in cm.
 \param ionization potential in eV.
*/
void StiMaterial::set(const string& name,
		      double z,
		      double a,
		      double density,
		      double radLength,
		      double ionization)
{
  setName(name);
  _density = density;
  _radLength = radLength;
  _a = a;
  _z = z;
  _ionization = ionization;
  if (_density>0)
    _x0 = _radLength/density;
  else
    _x0 = 0.;
  if (_a>0)
    _zOverA = _z/_a;
  else
    _zOverA = 0.;
}

ostream& operator<<(ostream& os, const StiMaterial& m)
{
  os << "StiMaterial:" << endl
     << "Name:"<< m.getName()
     << " Density:"<< m.getDensity()<< " g/cm^3"
     << " RadLength:"<<m.getRadLength()
     << " EffZ: "<<m.getZ()
     << " EffA: "<<m.getA()
     << " EffIoniz: "<<m.getIonization()
     << endl;
    
    return os;
}
