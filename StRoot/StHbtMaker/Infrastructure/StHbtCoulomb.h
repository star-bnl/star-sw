//////////////////////////////////////////////////////////////////////////////////
// This is a Coulomb correction class which
// 1. Reads in the dat from a file
// 2. Performs a linear interpolation in R and creates any array of interpolations
// 3. Interpolates in eta and returns the Coulomb correction to user
//
// - Randy Wells, OSU
//////////////////////////////////////////////////////////////////////////////////


#ifndef StHbtCoulomb_HH
#define StHbtCoulomb_HH

#include <stdio.h>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"

class StHbtCoulomb {

public:
  StHbtCoulomb();
  StHbtCoulomb(const char *readFile, const double& radius);
  virtual ~StHbtCoulomb();

  void SetRadius(const double& radius);
  double GetRadius();
  void SetFile(const char *readFile);
  double CoulombCorrect(const double& eta);
  double CoulombCorrect(const double& eta, const double& radius);
  double CoulombCorrect(const double& Z1Z2, const double& mass1,
			const double& mass2, const double& Qinv);
  double CoulombCorrect(const double& Z1Z2, const double& mass1,
			const double& mass2, const double& Qinv,
			const double& radius);
  double CoulombCorrect(const StHbtPair* pair, const double& charge);
  double CoulombCorrect(const StHbtPair* pair, const double& charge,
                        const double& radius);


private:
  double Eta(const double& Z1Z2, const double& mass1,
	     const double& mass2, const double& Qinv);  // Calculates eta
  void CreateLookupTable(const double& radius);  // Creates look-up table
  const char* mFile;                             // File to interpolate corrections from    
  double mRadius;                                // Radius from previous iteration
  double mEta[1000];                              // interpolated Coulomb correction table
  double mCoulomb[1000];                          // interpolated Coulomb correction table
  int mNLines;                                   // Number of Eta's in lookup-table

#ifdef __ROOT__ 
  ClassDef(StHbtCoulomb, 1)
#endif
};


#endif
