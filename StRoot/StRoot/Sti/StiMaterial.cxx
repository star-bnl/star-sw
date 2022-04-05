//StiMaterials.h
//M.L. Miller (Yale Software)
//04/01

//#include "StGetConfigValue.hh"
#include <assert.h>
#include <string.h>
#include <stdexcept>
#include "Sti/StiMaterial.h"
#include "Sti/StiElossCalculator.h"

StiMaterial::StiMaterial(){
} // StiMaterial()

StiMaterial::StiMaterial(const string &name,
			 double z,
			 double a,
			 double density,
			 double radLength,
			 double ionization)
{
  _eloss = 0;
  set(name,z,a,density,radLength,ionization);
}

StiMaterial::StiMaterial(const string &name,
			 double z,
			 double a,
			 double density,
			 double X0)
{
  _eloss = 0;
  set(name,z,a,density,X0);
}

StiMaterial::~StiMaterial()
{
 delete _eloss; _eloss = 0;

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
		      double radLength,//X0*density
		      double ionization)
{
  setName(name);
  _density = density;
  _a = a;
  _z = z;
  _ionization = ionization;
  _x0 = (_density>0)? radLength/density : 1e11;
  _zOverA = (_a>0)? _z/_a : 0;
  delete _eloss; _eloss = 0;
  if (z>0) 
    _eloss = new StiElossCalculator(_zOverA, ionization,_a,_z,_density);
  assert(_x0>0);

}

void StiMaterial::set(const string& name,
		      double z,
		      double a,
		      double density,
		      double X0)
{
  setName(name);
  _density = density;
  _a = a;
  _z = z;
  _x0 = X0;
  _zOverA = (_a>0)? _z/_a : 0;
  delete _eloss; _eloss = 0;
  if (z>0) 
    _eloss = new StiElossCalculator(_zOverA, getIonization(),_a,_z,_density);
  assert(_x0>0);


}
ostream& operator<<(ostream& os, const StiMaterial& m)
{
  os << "StiMaterial:" << endl
     << "Name:"		<< m.getName()
     << " Density:"	<< m.getDensity()<< " g/cm^3"
     << " X0:"		<<m.getX0()
     << " Z: "		<<m.getZ()
     << " A: "		<<m.getA()
     << endl;
    
    return os;
}
StiElossCalculator *StiMaterial::getElossCalculator() const 
{ if (_eloss) return _eloss;
  assert(strstr(getName().c_str(),"Vac"));
  return 0;
}
