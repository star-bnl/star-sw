/*
 * StiMaterial represents the physical characteristics of a material within
 * the ITTF geometry.
 */

#ifndef STI_MATERIAL_HH
#define STI_MATERIAL_HH

#include <iostream>
#include "TObject.h"
#include "THashTable.h"

class StiMaterial : public TObject{

  public:

  // con/destructor
  StiMaterial(){ 
    if(gMaterials == NULL){ gMaterials = new THashTable(); }
    StiMaterial::gMaterials->Add(this); }
  virtual ~StiMaterial(){ StiMaterial::gMaterials->Remove(this); }

  // accessors
  double getDensity() const { return dDensity; }
  double getRadLength() const { return dRadLength; }
  char* getName() const { return szName; }

  // mutators
  void setDensity(double val){ dDensity = val; }
  void setRadLength(double val){ dRadLength = val; }
  void setName(char *val){ szName = val; }

  // override hash for storing in static hashtable
  ULong_t Hash(){ TString str(szName); return str.Hash(); };
  static StiMaterial* findMaterial(const char *name) { 
    return (StiMaterial *)gMaterials->FindObject(name); }

  protected:

  double dDensity;   // g/cm^3
  double dRadLength; // cm
  char *szName;

  static THashTable *gMaterials;
};



//Non-members--------------------------

ostream& operator<<(ostream& os, const StiMaterial& m){
  return os << m.getDensity() << " " 
            << m.getRadLength() << " "
            << m.getName();
}

#endif




