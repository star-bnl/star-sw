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
    os << m.getDensity() << " " 
       << m.getRadLength() << " "
       << m.getName();
    return os;
}
