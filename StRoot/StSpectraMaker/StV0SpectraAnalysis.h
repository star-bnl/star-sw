/***************************************************************************
 *
 * $Id: StV0SpectraAnalysis.h,v 1.1 2000/03/23 03:21:50 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: Strangeness Spectra Analysis class
 *
 ***************************************************************************
 *
 * $Log: StV0SpectraAnalysis.h,v $
 * Revision 1.1  2000/03/23 03:21:50  munhoz
 * added V0 classes
 *
 **************************************************************************/

#ifndef StV0SpectraAnalysis_hh
#define StV0SpectraAnalysis_hh

class TH1D;
class TH2D;
class TH3D;

#include "StSpectraAnalysis.h"

class StV0SpectraAnalysis : public  StSpectraAnalysis {

 private:

  TH3D* m3DInvMass;          // 3-D histogram y(eta)-pt(mt)-Invariant Mass, total yield
  TH3D* m3DInvMassWeighted;  // 3-D histogram y(eta)-pt(mt)-Invariant Mass, weighted yield (efficiency)

  StParticleDefinition* mDaughterPos;  // Positive Daughter
  StParticleDefinition* mDaughterNeg;  // Negative Daughter

  float mlMassBin;  // Lower edge on Invariant Mass axis
  float muMassBin;  // Upper edge on Invariant Mass axis
  int   mnMassBins; // Number of bins on Invariant Mass axis

 protected:

 public:

  StV0SpectraAnalysis();
  ~StV0SpectraAnalysis();

  void setParticle(string particle);

  void bookHistograms();
  void fillHistograms(StEvent& event);
  void projectHistograms();
  void writeHistograms();

  void setZAxis(float lZbin=0., float uZbin=1.5, int nZbin=150);
};

#endif







