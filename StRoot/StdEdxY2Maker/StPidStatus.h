#ifndef __StPidStatus_h__
#define __StPidStatus_h__
#include "StProbPidTraits.h"
#include "StBTofPidTraits.h"
#include "StDedxPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StPicoEvent/StPicoTrack.h"
class StGlobalTrack;
class StdEdxStatus {
 public:
  StdEdxStatus(StDedxPidTraits *pid = 0) : fPiD(pid) {}
  virtual ~StdEdxStatus() {}
  StDedxPidTraits *fPiD; //!
  Double_t I() {return (fPiD) ? fPiD->mean() : 0;}
  Double_t D() {return (fPiD) ? fPiD->errorOnMean() : 0;}
  Double_t TrackLength() {return (fPiD) ? fPiD->length() : 0;}
  Double_t log2dX() {return (fPiD) ? fPiD->log2dX() : 0;}
  Int_t    N() {return (fPiD) ? fPiD->numberOfPoints() : 0;}
  Double_t Pred[KPidParticles];
  Double_t dev[KPidParticles];
  Double_t devS[KPidParticles];
};
class StToFStatus {
 public:
  StToFStatus(StBTofPidTraits *pid = 0) { fPiD = (pid && TMath::Abs(pid->yLocal()) < 1.8) ? pid :0;}
  virtual ~StToFStatus() {}
  StBTofPidTraits *fPiD; //!
  Double_t Sigma(Int_t l) {
    if (fPiD) {
      switch (l) {
      case kPidElectron:
	return fPiD->sigmaElectron();
      case kPidProton:
	return fPiD->sigmaProton();
      case kPidKaon:
	return fPiD->sigmaKaon();
      case kPidPion:
	return fPiD->sigmaPion();
      default:
	return 999.;
      }
    }
    return 999.;
  }
};

class StPidStatus {
 public:
  StPidStatus(StGlobalTrack *gTrack = 0);
  StPidStatus(StMuTrack *muTrack = 0);
  StPidStatus(StPicoTrack *picoTrack = 0);
  virtual ~StPidStatus() {}
  void Clear() {memset(mBeg,0,mEnd-mBeg+1);}
  Int_t Status() {return PiDStatus;}
  StdEdxStatus *Status(StDedxMethod k) {return fStatus[k];}
  void Set();
  StdEdxStatus fI70; //!
  StdEdxStatus fFit; //!
  StdEdxStatus fI70U; //!
  StdEdxStatus fFitU; //!
  StdEdxStatus fdNdx; //!
  StdEdxStatus fdNdxU;//!
  StToFStatus  fToF; //!
  Int_t        PiDStatus; //!
  StdEdxStatus* fStatus[kOtherMethodId2+1];
  //  StGlobalTrack *gTrack; //!
  StThreeVectorD g3; //!
  Char_t                mBeg[1];                   //!
  StProbPidTraits *fProb; //!
  Double_t devToF[KPidParticles];
  Int_t  PiDkey;    //! best
  Int_t  PiDkeyU;   //! only one with devZs<3, 
  Int_t  PiDkeyU3;  //! -"- and devZs > 5 for all others 
  Int_t  lBest;     //!
  Double_t dNdx[KPidParticles]; // no. of primary clusters per 1cm
  Double_t PredBMN[2], Pred70BMN[2]; //!
  Double_t bghyp[KPidParticles]; //! log10(bg)
  Double_t bgs[KPidParticles]; //! bg
  Char_t                mEnd[1];        //!
};

#endif 
