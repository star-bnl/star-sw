#include "StPidStatus.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StGlobalTrack.h"
#include "StTrackGeometry.h"
#include "TMath.h"
#include "TVector3.h"
//________________________________________________________________________________
StPidStatus::StPidStatus(StGlobalTrack *gTrack) : PiDStatus(-1) {// , gTrack(Track) {
  Clear();
  if (! gTrack) return;
  g3 = gTrack->geometry()->momentum(); // p of global track
  StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
  if (! traits.size()) return;
  for (UInt_t i = 0; i < traits.size(); i++) {
    StTrackPidTraits *trait = traits[i];
    if (! trait) continue;
    if ( trait->IsZombie()) continue;
    Short_t id = trait->detector()%100;
    if (id == kTofId) {
      StBTofPidTraits* pid = dynamic_cast<StBTofPidTraits*>(trait);
      if (! pid) continue;
      if (TMath::Abs(pid->yLocal()) < 1.8) fToF = StToFStatus(pid);
    } else if (id == kTpcId) {
      StDedxPidTraits* pid = dynamic_cast<StDedxPidTraits*>(trait);
      if (pid) {
	switch (pid->method()) {
	case kTruncatedMeanId: fI70 = StdEdxStatus(pid); break;
	case kLikelihoodFitId: fFit = StdEdxStatus(pid); break;
	case kEnsembleTruncatedMeanId: fI70U = StdEdxStatus(pid); break;// == kTruncatedMeanId+1 uncorrected
	case kWeightedTruncatedMeanId: fFitU = StdEdxStatus(pid); break;  // == kLikelihoodFitId+1; uncorrected
	case kOtherMethodId:           fdNdx = StdEdxStatus(pid); break;
	case kOtherMethodId2:          fdNdxU= StdEdxStatus(pid); break;
	default: break;
	}
      } else {
	StProbPidTraits *pidprob = dynamic_cast<StProbPidTraits*>(trait);
	if (pidprob) fProb = pidprob;
      }
    }
  }
  Set();
}
//________________________________________________________________________________
StPidStatus::StPidStatus(StMuTrack *muTrack) : PiDStatus(-1) {
  Clear();
  if (! muTrack) return;
  const StMuProbPidTraits &probPidTraits = muTrack->probPidTraits();
  //  const StMuBTofPidTraits &btofPidTraits = muTrack->btofPidTraits();
  //  const StMuMtdPidTraits  &mtdPidTraits  = muTrack->mtdPidTraits();
  g3 = muTrack->p(); // p of global track
  static StDedxPidTraits pidI70; //!
  static StDedxPidTraits pidFit; //!
  static StDedxPidTraits pidI70U; //!
  static StDedxPidTraits pidFitU; //!
  static StDedxPidTraits pidNdx; //!
  static StDedxPidTraits pidNdxU;//!
  //  static StBTofPidTraits pidToF; //!
  if (probPidTraits.dEdxTruncated() > 0) {
    pidI70 = StDedxPidTraits(kTpcId, kTruncatedMeanId, 100*((UShort_t)probPidTraits.dEdxTrackLength()) + muTrack->nHitsDedx(), 
			     probPidTraits.dEdxTruncated(), probPidTraits.dEdxErrorTruncated());
    fI70 = StdEdxStatus(&pidI70);
  } 
  if (probPidTraits.dEdxFit() > 0) {
    pidFit = StDedxPidTraits(kTpcId, kLikelihoodFitId, 100*((UShort_t)probPidTraits.dEdxTrackLength()) + muTrack->nHitsDedx(), 
			     probPidTraits.dEdxFit(), probPidTraits.dEdxErrorFit());
    fFit = StdEdxStatus(&pidFit);
  }
  if (probPidTraits.dNdxFit() > 0) {
    pidNdx = StDedxPidTraits(kTpcId, kOtherMethodId, 100*((UShort_t)probPidTraits.dEdxTrackLength()) + muTrack->nHitsDedx(), 
			     probPidTraits.dNdxFit(), probPidTraits.dNdxErrorFit());
    fdNdx = StdEdxStatus(&pidNdx);
  }
  Set();
}
//________________________________________________________________________________
StPidStatus::StPidStatus(StPicoTrack *picoTrack) : PiDStatus(-1) {
  Clear();
  if (! picoTrack) return;
  TVector3 gMom = picoTrack->gMom();
  g3 = StThreeVectorF(gMom.X(), gMom.Y(), gMom.Z()); // p of global track
  static StDedxPidTraits pidI70; //!
  static StDedxPidTraits pidFit; //!
  static StDedxPidTraits pidI70U; //!
  static StDedxPidTraits pidFitU; //!
  static StDedxPidTraits pidNdx; //!
  static StDedxPidTraits pidNdxU;//!
  pidI70 = StDedxPidTraits(kTpcId, kTruncatedMeanId, picoTrack->nHitsDedx(), 
			   1e-6*picoTrack->dEdx(), picoTrack->dEdxError());
  fI70 = StdEdxStatus(&pidI70);
  pidFit = StDedxPidTraits(kTpcId, kLikelihoodFitId, picoTrack->nHitsDedx(), 
			   1e-6*picoTrack->dEdx(), picoTrack->dEdxError());
  fFit = StdEdxStatus(&pidFit);
  Set();
}
//________________________________________________________________________________
void StPidStatus::Set() {
  if (! fI70.fPiD && ! fFit.fPiD && ! fdNdx.fPiD) return;
  PiDStatus = 0;
  Double_t pMomentum = g3.mag();
  //  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Int_t l;
  PredBMN[0] = Pred70BMN[0] =  1;
  PredBMN[1] = Pred70BMN[1] = -1;
  memset (fStatus, 0, sizeof(fStatus));
  fStatus[kTruncatedMeanId]         = fI70.fPiD   ? &fI70   : 0;
  fStatus[kLikelihoodFitId]         = fFit.fPiD   ? &fFit   : 0;
  fStatus[kEnsembleTruncatedMeanId] = fI70U.fPiD  ? &fI70U  : 0;
  fStatus[kWeightedTruncatedMeanId] = fFitU.fPiD  ? &fFitU  : 0;
  fStatus[kOtherMethodId]           = fdNdx.fPiD  ? &fdNdx  : 0;
  fStatus[kOtherMethodId2]          = fdNdxU.fPiD ? &fdNdxU : 0;

  for (l = kPidElectron; l < KPidParticles; l++) {
    Double_t charge = StProbPidTraits::mPidParticleDefinitions[l]->charge();
    Double_t mass   = StProbPidTraits::mPidParticleDefinitions[l]->mass();
    bgs[l]   = pMomentum*TMath::Abs(charge)/mass;
    bghyp[l] = TMath::Log10(bgs[l]);
    for (Int_t m = 1; m <= kOtherMethodId2; m++) {
      if (! fStatus[m]) continue;
      switch (m) {
      case kTruncatedMeanId: 
      case kEnsembleTruncatedMeanId: 
	fStatus[m]->Pred[l] = 1.e-6*charge*charge*
	  Bichsel::Instance()->GetI70M(bghyp[l],fStatus[m]->fPiD->log2dX());
	  break;
      case kLikelihoodFitId: 
      case kWeightedTruncatedMeanId:
	fStatus[m]->Pred[l] = 1.e-6*charge*charge*
	  TMath::Exp(Bichsel::Instance()->GetMostProbableZ(bghyp[l],fStatus[m]->fPiD->log2dX())); 
	break;
      case kOtherMethodId: 
      case kOtherMethodId2: 
	fStatus[m]->Pred[l]  = StdEdxModel::instance()->dNdx(bgs[l], charge);
	break;
      default: continue;
      }
      if (fStatus[m]->I() > 0) {
	fStatus[m]->dev[l] = TMath::Log(fStatus[m]->I()/fStatus[m]->Pred[l]);
	fStatus[m]->devS[l] = -999;
	if (fStatus[m]->D() > 0) {
	  fStatus[m]->devS[l] = fStatus[m]->dev[l]/fStatus[m]->D();
	}
      }
      if (fStatus[m]->Pred[l] < PredBMN[0]) PredBMN[0] = fStatus[m]->Pred[l];
      if (fStatus[m]->Pred[l] > PredBMN[1]) PredBMN[1] = fStatus[m]->Pred[l];
    }
  }
  PiDkey    = -1; // best
  PiDkeyU   = -1; // only one with devZs<3, 
  PiDkeyU3  = -1; // -"- and devZs > 5 for all others 
  lBest     = -1;
#if 0
  Int_t lBestToF = -1;
  // use ToF 
  Double_t devZmin = 999;
  if (fToF.fPiD) {
    for (l = kPidElectron; l < KPidParticles; l++) {
      if      (l == kPidMuon    ) devToF[l] = devToF[kPidPion];
      else if (l >= kPidDeuteron) devToF[l] = devToF[kPidProton];
      else                        devToF[l] = TMath::Abs(fToF.Sigma(l));
    }
    for (l = kPidElectron; l < KPidParticles; l++) {
      if (devToF[l] < 3.0) PiDStatus |= 1<<l;
      if (devToF[l] < devZmin) {devZmin = devToF[l]; lBestToF = l;}
    }
    if (devZmin > 5) {lBestToF = -1;}
  }
  devZmin = 999;
  for (l = kPidElectron; l < KPidParticles; l++) {
    if (devZs[l] < 3.0) PiDStatus |= 1<<l;
    Double_t dev = devZs[l]*devZs[l];
    if (lBestToF >= 0) dev += devToF[l]*devToF[l];
    if (dev < devZmin) {
      lBest = l;
      devZmin = dev;
    }
  }
  if (lBest >=0) {
    if (devZs[lBest] < 3.0) {
      PiDkey = lBest;
      Int_t lNext = -1;
      devZmin = 999;
      for (l = kPidElectron; l < KPidParticles; l++) {
	if (l == lBest) continue;
	Double_t dev = devZs[l]*devZs[l];
	if (lBestToF >= 0) dev += devToF[l]*devToF[l];
	if (dev < devZmin) {devZmin = dev; lNext = l;}
      }
      if (lNext >= 0) {
	Double_t dev = devZs[lNext]*devZs[lNext];
	if (lBestToF >= 0) dev += devToF[lNext]*devToF[lNext];
	if (dev > 9.) {
	  PiDkeyU = PiDkey;
	  if (dev > 25.) PiDkeyU3 = PiDkeyU;
	}
      }
    }
  }
#endif
}
