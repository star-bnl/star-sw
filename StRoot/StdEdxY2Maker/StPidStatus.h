#ifndef __StPidStatus_h__
#define __StPidStatus_h__
#include "StProbPidTraits.h"
#include "StBTofPidTraits.h"
#include "StETofPidTraits.h"
#include "StMtdPidTraits.h"
#include "StDedxPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuETofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"
#ifdef __TFG__VERSION__
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoEvent/StPicoETofPidTraits.h"
#include "StPicoEvent/StPicoMtdPidTraits.h"
#endif /* __TFG__VERSION__ */
class StGlobalTrack;
class StdEdxStatus {
 public:
 StdEdxStatus(StDedxPidTraits *pid = 0) : fPiD(pid) {Clear();}
  virtual ~StdEdxStatus() {}
  StDedxPidTraits *fPiD; //!
  Double_t I() const {return (fPiD) ? fPiD->mean() : 0;}
  Double_t D() const {return (fPiD) ? fPiD->errorOnMean() : 0;}
  Double_t TrackLength() const {return (fPiD) ? fPiD->length() : 0;}
  Double_t log2dX() const {return (fPiD) ? fPiD->log2dX() : 0;}
  Int_t    N() const {return (fPiD) ? fPiD->numberOfPoints() : 0;}
  void Clear() {memset(mBeg,0,mEnd-mBeg+1);}
  void Print(Option_t *option = "") const;
  Char_t                mBeg[1];                   //!
  Double_t Pred[KPidParticles];
  Double_t dev[KPidParticles];
  Double_t devS[KPidParticles];
  Char_t                mEnd[1];        //!
};
class StBTofStatus {
 public:
  StBTofStatus(StBTofPidTraits *pid ) { fPiD = (TMath::Abs(pid->yLocal()) < 1.8) ? pid : 0;}
  virtual ~StBTofStatus() {}
  StBTofPidTraits *fPiD; //!
  StBTofPidTraits *PiD() {return fPiD;}
  Float_t beta() {return fPiD ? fPiD->beta() : -999;}
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
class StETofStatus {
 public:
  StETofStatus(StETofPidTraits *pid = 0) { fPiD = (pid && pid->matchFlag()) ? pid : 0;}
  virtual ~StETofStatus() {}
  StETofPidTraits *PiD() {return fPiD;}
  StETofPidTraits *fPiD; //!
  Float_t beta() {return fPiD ? fPiD->beta() : -999;}
};
class StMtdStatus {
 public:
  StMtdStatus(StMtdPidTraits *pid = 0) { fPiD = (pid && pid->matchFlag()) ? pid : 0;}
  virtual ~StMtdStatus() {}
  StMtdPidTraits *PiD() {return fPiD;}
  StMtdPidTraits *fPiD; //!
  Float_t beta() {return fPiD ? fPiD->beta() : -999;}
};

class StPidStatus {
 public:
  enum PiDStatusIDs {
    kI70,   kFit,   kI70U,   kFitU,   kdNdx,   kdNdxU,  kBTof,   kETof,   kMtd, kTotal
  };
  StPidStatus(StGlobalTrack *gTrack = 0, Bool_t Usedx2 = kTRUE);
  StPidStatus(StMuTrack *muTrack = 0, Bool_t Usedx2 = kTRUE);
#ifdef __TFG__VERSION__
  StPidStatus(StPicoTrack *picoTrack = 0, Bool_t Usedx2 = kFALSE);
#endif /* __TFG__VERSION__ */
  virtual ~StPidStatus() {
    SafeDelete(fProb); 
    SafeDelete(fI70); 
    SafeDelete(fFit); 
    SafeDelete(fI70U); 
    SafeDelete(fFitU); 
    SafeDelete(fdNdx); 
    SafeDelete(fdNdxU);
    SafeDelete(fBTof); 
    SafeDelete(fETof); 
    SafeDelete(fMtd); 
  }
  void Clear() {memset(mBeg,0,mEnd-mBeg+1);}
  Int_t Status() {return PiDStatus;}
  StdEdxStatus   *dEdxStatus(StDedxMethod k) {return fStatus[k];}
  StBTofPidTraits SetBTofPidTraits(const StMuBTofPidTraits &pid);
  StETofPidTraits SetETofPidTraits(const StMuETofPidTraits &pid);
  StMtdPidTraits  SetMtdPidTraits(const StMuMtdPidTraits &pid);
#ifdef __TFG__VERSION__
  StBTofPidTraits SetBTofPidTraits(const StPicoBTofPidTraits &pid);
  StETofPidTraits SetETofPidTraits(const StPicoETofPidTraits &pid);
  StMtdPidTraits  SetMtdPidTraits(const StPicoMtdPidTraits &pid);
#endif /* __TFG__VERSION__ */
  void Set();
  
  Int_t        PiDStatus; //!
  void        Print(Option_t *option="") const;
  //  StGlobalTrack *gTrack; //!
  StThreeVectorD g3; //!
  Bool_t fUsedx2;
  Char_t                mBeg[1];                   //!
  StProbPidTraits *fProb; //!
  StdEdxStatus *fStatus[kOtherMethodId2+1];
  StdEdxStatus *fI70;
  StdEdxStatus *fFit; //!
  StdEdxStatus *fI70U; //!
  StdEdxStatus *fFitU; //!
  StdEdxStatus *fdNdx; //!
  StdEdxStatus *fdNdxU;//!
  StBTofStatus *fBTof; //!
  StETofStatus *fETof; //!
  StMtdStatus  *fMtd; //!
  Double_t devTof[KPidParticles];
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
