//StiMaterials.h
//M.L. Miller (Yale Software)
//04/01

#include "StiMaterial.h"
#include "StGetConfigValue.hh"
#include <string.h>

StiMaterial::StiMaterial(){
} // StiMaterial()

StiMaterial::~StiMaterial(){
} // ~StiMaterial()

// reads object to config file
void StiMaterial::build(const char *szFileName){

    string str;
    StGetConfigValue(szFileName, "name", str);
    szName = str.c_str();

    StGetConfigValue(szFileName, "density", dDensity);
    StGetConfigValue(szFileName, "radLength", dRadLength);

} // build()

// writes object to config file
void StiMaterial::write(const char *szFileName){

  ofstream os(szFileName);
  
  os << "name:\t\t" << szName << endl;
  os << "density:\t" << dDensity << endl;
  os << "radLength:\t" << dRadLength << endl;

} // write()

ostream& operator<<(ostream& os, const StiMaterial& m){
  return os << m.getDensity() << " " 
            << m.getRadLength() << " "
            << m.getName();
}
