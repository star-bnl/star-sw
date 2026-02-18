#ifndef RHICFFIELD_H
#define RHICFFIELD_H 1

#include "G4MagneticField.hh"
#include "G4PhysicalConstants.hh"
//#include "G4SystemOfUnits.hh"

#include "globals.hh"

class RHICfField: public G4MagneticField
{
public:
  RHICfField();
  ~RHICfField();

  void GetFieldValue(const double Point[3],double *Bfield) const;

private:
};
#endif
