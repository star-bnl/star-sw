//StiMaterials.h
//M.L. Miller (Yale Software)
//04/01

#include "StGetConfigValue.hh"
#include <string.h>
#include "StiMaterial.h"

StiMaterial::StiMaterial(){
} // StiMaterial()

StiMaterial::~StiMaterial(){
} // ~StiMaterial()

ostream& operator<<(ostream& os, const StiMaterial& m)
{
  os << "Name:"<< m.getName()
     << " Density:"<< m.getDensity()
     << " RadLength:"<<m.getRadLength()
     << " EffZ: "<<m.getZ()
     << " EffA: "<<m.getA()
     << " EffIoniz: "<<m.getIonization()<<endl;
    
    return os;
}
