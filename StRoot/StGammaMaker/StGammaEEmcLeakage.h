#ifndef __StGammaEEmcLeakage_h__
#define __StGammaEEmcLeakage_h__

#include <TObject.h>
#include <TVector3.h>
#include <TCanvas.h>
#include <vector>

class TFile;
class TH2F;
class EEmcGeomSimple;
class StGammaTower;

class StGammaEEmcLeakage : public TObject
{

 public:

  StGammaEEmcLeakage();
  ~StGammaEEmcLeakage();

  /// Given a vector pointing to the gamma candidate's position
  /// on the calorimeter, returns the expected fraction of the 
  /// photon's energy which will be contained in the tower whose
  /// center is at tower.
  Float_t expectation( const TVector3 &gamma );

  /// Function to draw the photon's location projected onto the plane
  /// where the energy leakage is evaluated.  This memory leaks all over
  /// the place, so don't go crazy with this.
  TCanvas *draw( const TVector3 &gamma );

  static StGammaEEmcLeakage *instance();

 private:
 protected:
  
  TFile *mFile;
  Int_t  mNumberOfEtabins;
  std::vector< TH2F* > mEnergyFractions;
  EEmcGeomSimple *mEEmcGeom;

  // Class implemented as a singleton
  static StGammaEEmcLeakage *sInstance;

  ClassDef(StGammaEEmcLeakage,1);

}; 
#endif
