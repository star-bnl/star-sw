#ifndef StHbtCoulomb_HH
#define StHbtCoulomb_HH

#include <stdio.h>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"

class StHbtCoulomb {

public:
  StHbtCoulomb();
  StHbtCoulomb(const char *readFile, const double& radius,
	       const double& charge);
  virtual ~StHbtCoulomb();

  void SetRadius(const double& radius);
  double GetRadius();
  void SetFile(const char *readFile);
  void SetChargeProduct(const double& charge);
  void SetMass(const double& mass1, const double& mass2);

  // These have different names so eta/Qinv don't confuse the compiler
  double Correction(const double& eta);
  double Correction(const double& eta, const double& radius);
  double CoulombCorrect(const double& Qinv);
  double CoulombCorrect(const double& Qinv, const double& radius);
  double CoulombCorrect(const double& charge, const double& mass1,
			const double& mass2, const double& Qinv);
  double CoulombCorrect(const double& charge, const double& mass1,
			const double& mass2, const double& Qinv,
			const double& radius);
  double CoulombCorrect(const StHbtPair* pair);
  double CoulombCorrect(const StHbtPair* pair, const double& radius);


private:
  double Eta(const double& Qinv);                // Calculates eta
  void CreateLookupTable(const double& radius);  // Creates look-up table
  const char* mFile;                             // File to interpolate corrections from    
  double mRadius;                                // Radius from previous iteration
  double mZ1Z2;                                  // Charge product of particles
  double mMass1;                                 // Mass of 1st particle
  double mMass2;                                 // Mass of 2nd particle
  double mEta[1000];                             // interpolated Coulomb correction table
  double mCoulomb[1000];                         // interpolated Coulomb correction table
  int mNLines;                                   // Number of Eta's in lookup-table

#ifdef __ROOT__
  ClassDef(StHbtCoulomb, 1)
#endif
};


#endif
