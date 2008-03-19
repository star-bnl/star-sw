#ifndef StMuPrimaryTrackCovariance_h
#define StMuPrimaryTrackCovariance_h
#include "TObject.h"
#include "StMatrixF.hh"

class StMuPrimaryTrackCovariance : public TObject {
 public:
  StMuPrimaryTrackCovariance();
  StMuPrimaryTrackCovariance(StMatrixF cov);
  StMuPrimaryTrackCovariance(const Float_t *array);
  virtual ~StMuPrimaryTrackCovariance() {}
  const Float_t*     errMatrix() const {return &mTanTan;}
 private:
  Char_t   mBeg[1];//!
  /// pars errors
  Float_t  mTanTan;                   /* Psi = azimuthal angle of pT vector (deg)     */         
  Float_t  mPsiTan, mPsiPsi;	      /* Tan = tan(dip) =pz/pt at start               */
  Float_t  mPtiTan, mPtiPsi, mPtiPti; /* Pti = 1/pt at start (GeV/c)^(-1)             */
  Char_t   mEnd[1];//!
  ClassDef(StMuPrimaryTrackCovariance,1)
    };
#endif
