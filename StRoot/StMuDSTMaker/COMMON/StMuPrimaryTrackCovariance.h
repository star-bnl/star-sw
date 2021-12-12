#ifndef StMuPrimaryTrackCovariance_h
#define StMuPrimaryTrackCovariance_h
#include "TObject.h"
#include "StMatrixF.hh"

#ifdef __TFG__VERSION__
#include "TArrayF.h"
#endif /* __TFG__VERSION__ */
class StMuPrimaryTrackCovariance : public TObject {
 public:
  StMuPrimaryTrackCovariance();
  StMuPrimaryTrackCovariance(StMatrixF cov);
#ifndef __TFG__VERSION__
  StMuPrimaryTrackCovariance(const Float_t *array);
#else /* __TFG__VERSION__ */
  StMuPrimaryTrackCovariance(const TArrayF &array);
#endif /* __TFG__VERSION__ */
  virtual ~StMuPrimaryTrackCovariance() {}
  const Float_t*     errMatrix() const {return &mTanTan;}
  virtual void Print(Option_t* option = "") const;
 private:
  Char_t   mBeg[1];//!
  /// pars errors
  Float_t  mTanTan;                   /* Psi = azimuthal angle of pT vector (deg)     */         
  Float_t  mPsiTan, mPsiPsi;	      /* Tan = tan(dip) =pz/pt at start               */
#ifndef __TFG__VERSION__
  Float_t  mPtiTan, mPtiPsi, mPtiPti; /* Pti = 1/pt at start (GeV/c)^(-1)             */
#else /* __TFG__VERSION__ */
  Float_t  mPtiTan, mPtiPsi, mPtiPti; /* Pti = -q/pt at start (GeV/c)^(-1)            */
#endif /* __TFG__VERSION__ */
  Char_t   mEnd[1];//!
  ClassDef(StMuPrimaryTrackCovariance,1)
};
ostream&              operator<<(ostream& os,  const StMuPrimaryTrackCovariance& v);
#endif
