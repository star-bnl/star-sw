/*
 * StiMaterial represents the physical characteristics of a material within
 * the ITTF geometry.
 */

#ifndef STI_MATERIAL_HH
#define STI_MATERIAL_HH

#include <iostream>

class StiMaterial{

  public:

  // con/destructor
  StiMaterial();
  virtual ~StiMaterial();

  // accessors
  double getDensity() const { return dDensity; }
  double getRadLength() const { return dRadLength; }
  const char* getName() const { return szName; }

  // mutators
  void setDensity(double val){ dDensity = val; }
  void setRadLength(double val){ dRadLength = val; }
  void setName(const char *val){ strncpy(szName, val, 99); }

  // utility
  void build(const char *szFileName);
  void write(const char *szFileName);

  protected:

  double dDensity;   // g/cm^3
  double dRadLength; // cm
  char szName[100];

};

//Non-members--------------------------

ostream& operator<<(ostream& os, const StiMaterial& m);

#endif




