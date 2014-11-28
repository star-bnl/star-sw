/***************************************************************************
 *
 * $Id: 
 *
 * Author: Adam Kisiel, Warsaw University of Technology, Poland
 ***************************************************************************
 *
 * Description : Calculate correlation weight using R.Lednicky's code 
 *               with the possibility to specify particle purity
 *  Use the fortran files : FsiLednickyWeight.F and FsiTools.F
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtFsiLednickyPurity_hh
#define StHbtFsiLednickyPurity_hh

#include "TRandom.h"
#include "StHbtMaker/Base/StHbtFsiWeight.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/ThCorrFctn/StHbtFsiLednicky.h"

class StHbtFsiLednickyPurity : public StHbtFsiLednicky {
 public: 
// --- Constructor
  StHbtFsiLednickyPurity(); // call SetDefaultCalcPar
// --- Destructor : nothing to explicitly delete
  ~StHbtFsiLednickyPurity();

// --- Function to be called in the correlation function
  // see StThCorrFctn for details
  virtual double GetWeight(const StHbtThPair* aThPair);

// --- Setting

// >>> Calculation mode
  void SetPurity(const double aPurity);
  void SetPurity(const double aPurity1, const double aPurity2);

  virtual StHbtString Report();

protected:

  // Particle purity
  double mPurity1, mPurity2;

  TRandom mRandom;

#ifdef __ROOT__
  ClassDef(StHbtFsiLednickyPurity,1)
#endif
};

#endif
