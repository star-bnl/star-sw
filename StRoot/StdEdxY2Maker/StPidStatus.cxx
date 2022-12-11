#ifdef __TFG__VERSION__
#include "StPicoEvent/StPicoDst.h"
#endif /*  __TFG__VERSION__ */
#include "StPidStatus.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StBichsel/StdEdxPull.h"
#include "StGlobalTrack.h"
#include "StTrackGeometry.h"

#include "TMath.h"
#include "TVector3.h"
//________________________________________________________________________________
StBTofPidTraits StPidStatus::SetBTofPidTraits(const StMuBTofPidTraits &pid) {
  StBTofPidTraits btofPidTraits;
  btofPidTraits.setMatchFlag    (pid.matchFlag());
  btofPidTraits.setYLocal       (pid.yLocal());
  btofPidTraits.setZLocal       (pid.zLocal());
  btofPidTraits.setThetaLocal   (pid.thetaLocal());
  btofPidTraits.setTimeOfFlight (pid.timeOfFlight());
  btofPidTraits.setPathLength   (pid.pathLength());
  btofPidTraits.setBeta         (pid.beta());

  btofPidTraits.setPosition     (pid.position());

  btofPidTraits.setSigmaElectron(pid.sigmaElectron()); 
  btofPidTraits.setSigmaPion    (pid.sigmaPion()); 
  btofPidTraits.setSigmaKaon    (pid.sigmaKaon()); 
  btofPidTraits.setSigmaProton  (pid.sigmaProton());  
  btofPidTraits.setProbElectron (pid.probElectron()); 
  btofPidTraits.setProbPion     (pid.probPion()); 
  btofPidTraits.setProbKaon     (pid.probKaon()); 
  btofPidTraits.setProbProton   (pid.probProton());  
  return btofPidTraits;
}
#ifdef __TFG__VERSION__
//________________________________________________________________________________
StBTofPidTraits StPidStatus::SetBTofPidTraits(const StPicoBTofPidTraits &pid) {
  StBTofPidTraits btofPidTraits;
  btofPidTraits.setMatchFlag    (pid.btofMatchFlag());
  btofPidTraits.setYLocal       (pid.btofYLocal());
  btofPidTraits.setZLocal       (pid.btofZLocal());
  //  btofPidTraits.setThetaLocal   (pid.btofThetaLocal());
  //  btofPidTraits.setTimeOfFlight (pid.btofTimeOfFlight());
  //  btofPidTraits.setPathLength   (pid.btofPathLength());
  btofPidTraits.setBeta         (pid.btofBeta());
  btofPidTraits.setPosition     (StThreeVectorF(pid.btofHitPosX(),pid.btofHitPosY(),pid.btofHitPosZ()));
				      
  btofPidTraits.setSigmaElectron(pid.nSigmaElectron()); 
  btofPidTraits.setSigmaPion    (pid.nSigmaPion()); 
  btofPidTraits.setSigmaKaon    (pid.nSigmaKaon()); 
  btofPidTraits.setSigmaProton  (pid.nSigmaProton());  
  //  btofPidTraits.setProbElectron (pid.btofProbElectron()); 
  //  btofPidTraits.setProbPion     (pid.btofProbPion()); 
  //  btofPidTraits.setProbKaon     (pid.btofProbKaon()); 
  //  btofPidTraits.setProbProton   (pid.btofProbProton());  
  return btofPidTraits;
}
#endif /*  __TFG__VERSION__ */				      
//________________________________________________________________________________
StETofPidTraits StPidStatus::SetETofPidTraits(const StMuETofPidTraits &pid) {
  StETofPidTraits etofPidTraits;
  etofPidTraits.setMatchFlag(   pid.matchFlag   ());
  etofPidTraits.setLocalX(      pid.localX      ());
  etofPidTraits.setLocalY(      pid.localY      ());
  etofPidTraits.setThetaLocal(  pid.thetaLocal  ());
  etofPidTraits.setPosition(    pid.position    ());
  etofPidTraits.setDeltaX(      pid.deltaX      ());
  etofPidTraits.setDeltaY(      pid.deltaY      ());
  etofPidTraits.setTimeOfFlight(pid.timeOfFlight());
  etofPidTraits.setPathLength(  pid.pathLength  ());
  etofPidTraits.setBeta(        pid.beta        ());
  return etofPidTraits;
}
#ifdef __TFG__VERSION__
//________________________________________________________________________________
StETofPidTraits StPidStatus::SetETofPidTraits(const StPicoETofPidTraits &pid) {
  StETofPidTraits etofPidTraits;
  etofPidTraits.setMatchFlag(   pid.matchFlag   ());
  //  etofPidTraits.setLocalX(      pid.localX      ());
  //  etofPidTraits.setLocalY(      pid.localY      ());
  //  etofPidTraits.setThetaLocal(  pid.thetaLocal  ());
  etofPidTraits.setPosition     (StThreeVectorF(pid.crossingX(),pid.crossingY(),pid.crossingZ()));
  etofPidTraits.setDeltaX(      pid.deltaX      ());
  etofPidTraits.setDeltaY(      pid.deltaY      ());
  etofPidTraits.setTimeOfFlight(pid.tof         ());
  //  etofPidTraits.setPathLength(  pid.pathLength  ());
  etofPidTraits.setBeta(        pid.beta        ());
  return etofPidTraits;
}
//________________________________________________________________________________
StMtdPidTraits StPidStatus::SetMtdPidTraits(const StPicoMtdPidTraits &pid) {
  StMtdPidTraits mtdPidTraits;
  mtdPidTraits.setMatchFlag      (pid.matchFlag());
//   mtdPidTraits.setYLocal         (pid.yLocal());
//   mtdPidTraits.setZLocal         (pid.zLocal());
  mtdPidTraits.setDeltaY         (pid.deltaY());
  mtdPidTraits.setDeltaZ         (pid.deltaZ());
  //  mtdPidTraits.setThetaLocal     (pid.thetaLocal());
  //  mtdPidTraits.setTimeOfFlight   (pid.timeOfFlight());
  //  mtdPidTraits.setExpTimeOfFlight(pid.expTimeOfFlight());
  //  mtdPidTraits.setPathLength     (pid.pathLength());
  mtdPidTraits.setBeta           (pid.beta());
  //  mtdPidTraits.setPosition       (pid.position());

  //  mtdPidTraits.setSigmaMuon      (pid.sigmaMuon()); 
  //  mtdPidTraits.setProbMuon       (pid.probMuon()); 
  return mtdPidTraits;
}
#endif /*  __TFG__VERSION__ */
//________________________________________________________________________________
StMtdPidTraits StPidStatus::SetMtdPidTraits(const StMuMtdPidTraits &pid) {
  StMtdPidTraits mtdPidTraits;
  mtdPidTraits.setMatchFlag      (pid.matchFlag());
  mtdPidTraits.setYLocal         (pid.yLocal());
  mtdPidTraits.setZLocal         (pid.zLocal());
  mtdPidTraits.setDeltaY         (pid.deltaY());
  mtdPidTraits.setDeltaZ         (pid.deltaZ());
  mtdPidTraits.setThetaLocal     (pid.thetaLocal());
  mtdPidTraits.setTimeOfFlight   (pid.timeOfFlight());
  mtdPidTraits.setExpTimeOfFlight(pid.expTimeOfFlight());
  mtdPidTraits.setPathLength     (pid.pathLength());
  mtdPidTraits.setBeta           (pid.beta());
  mtdPidTraits.setPosition       (pid.position());

  mtdPidTraits.setSigmaMuon      (pid.sigmaMuon()); 
  mtdPidTraits.setProbMuon       (pid.probMuon()); 
  return mtdPidTraits;
}
//________________________________________________________________________________
StPidStatus::StPidStatus(StGlobalTrack *gTrack, Bool_t Usedx2) : PiDStatus(-1), fUsedx2(Usedx2) {// , gTrack(Track) {
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
    if (id == kBTofId) {
      StBTofPidTraits* pid = dynamic_cast<StBTofPidTraits*>(trait);
      if (! pid) continue;
      if (TMath::Abs(pid->yLocal()) < 1.8) fBTof = new StBTofStatus(pid);
    } else if (id == kETofId) {
      StETofPidTraits* pid = dynamic_cast<StETofPidTraits*>(trait);
      if (! pid) continue;
      fETof = new StETofStatus(pid);
    } else if (id == kMtdId) {
      StMtdPidTraits* pid = dynamic_cast<StMtdPidTraits*>(trait);
      if (! pid) continue;
      fMtd = new StMtdStatus(pid);
    } else if (id == kTpcId) {
      StDedxPidTraits* pid = dynamic_cast<StDedxPidTraits*>(trait);
      if (pid) {
	switch (pid->method()) {
	case kTruncatedMeanId: fI70 = new StdEdxStatus(pid); break;
	case kLikelihoodFitId: fFit = new StdEdxStatus(pid); break;
	case kEnsembleTruncatedMeanId: fI70U = new StdEdxStatus(pid); break;// == kTruncatedMeanId+1 uncorrected
	case kWeightedTruncatedMeanId: fFitU = new StdEdxStatus(pid); break;  // == kLikelihoodFitId+1; uncorrected
	case kOtherMethodId:           fdNdx = new StdEdxStatus(pid); break;
	case kOtherMethodId2:          fdNdxU = new StdEdxStatus(pid); break;
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
StPidStatus::StPidStatus(StMuTrack *muTrack, Bool_t Usedx2) : PiDStatus(-1), fUsedx2(Usedx2) {
  Clear();
  if (! muTrack) return;
  const StMuProbPidTraits &probPidTraits = muTrack->probPidTraits();
  const StMuBTofPidTraits &btofPidTraits = muTrack->btofPidTraits();
  const StMuETofPidTraits &etofPidTraits = muTrack->etofPidTraits();
  const StMuMtdPidTraits  &mtdPidTraits = muTrack->mtdPidTraits();
  g3 = muTrack->p(); // p of global track
  static StDedxPidTraits pidI70; //!
  static StDedxPidTraits pidFit; //!
  static StDedxPidTraits pidI70U; //!
  static StDedxPidTraits pidFitU; //!
  static StDedxPidTraits pidNdx; //!
  static StDedxPidTraits pidNdxU;//!
  static StBTofPidTraits pidBTof; //!
  static StETofPidTraits pidETof; //!
  static StMtdPidTraits  pidMtd; //!
  if (probPidTraits.dEdxTruncated() > 0) {
    pidI70 = StDedxPidTraits(kTpcId, kTruncatedMeanId, 100*((UShort_t)probPidTraits.dEdxTrackLength()) + muTrack->nHitsDedx(), 
			     probPidTraits.dEdxTruncated(), probPidTraits.dEdxErrorTruncated(),probPidTraits.log2dX());
    fI70 = new StdEdxStatus(&pidI70);
  } 
  if (probPidTraits.dEdxFit() > 0) {
    pidFit = StDedxPidTraits(kTpcId, kLikelihoodFitId, 100*((UShort_t)probPidTraits.dEdxTrackLength()) + muTrack->nHitsDedx(), 
			     probPidTraits.dEdxFit(), probPidTraits.dEdxErrorFit(),probPidTraits.log2dX());
    fFit = new StdEdxStatus(&pidFit);
  }
  if (probPidTraits.dNdxFit() > 0) {
    pidNdx = StDedxPidTraits(kTpcId, kOtherMethodId, 100*((UShort_t)probPidTraits.dEdxTrackLength()) + muTrack->nHitsDedx(), 
			     probPidTraits.dNdxFit(), probPidTraits.dNdxErrorFit(),probPidTraits.log2dX());
    fdNdx = new StdEdxStatus(&pidNdx);
  }
  if (btofPidTraits.matchFlag()) {
    static StBTofPidTraits btof;
    btof= SetBTofPidTraits(muTrack->btofPidTraits());
    fBTof = new StBTofStatus(&btof);
  }
  if (etofPidTraits.matchFlag()) {
    static StETofPidTraits etof;
    etof = SetETofPidTraits(muTrack->etofPidTraits());
    fETof = new StETofStatus(&etof);
  }
  if (mtdPidTraits.matchFlag()) {
    StMtdPidTraits mtd;
    mtd = SetMtdPidTraits(muTrack->mtdPidTraits());
    fMtd = new StMtdStatus(&mtd);
  }

  Set();
}
#ifdef __TFG__VERSION__
//________________________________________________________________________________
StPidStatus::StPidStatus(StPicoTrack *picoTrack, Bool_t Usedx2) : PiDStatus(-1), fUsedx2(Usedx2) {
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
  static StBTofPidTraits pidBTof; //!
  static StETofPidTraits pidETof; //!
  static StMtdPidTraits  pidMtd; //!
  if (picoTrack->dEdx() > 0) {
#if 0 /* no I70 on picoDst */
    pidI70 = StDedxPidTraits(kTpcId, kTruncatedMeanId, picoTrack->nHitsDedx(), 
			     1e-6*picoTrack->dEdx(), picoTrack->dEdxError());
    fI70 = new StdEdxStatus(&pidI70);
#endif
    pidFit = StDedxPidTraits(kTpcId, kLikelihoodFitId, picoTrack->nHitsDedx(), 
			     1e-6*picoTrack->dEdx(), picoTrack->dEdxError());
    fFit = new StdEdxStatus(&pidFit);
  }
  if (picoTrack->dNdx() > 0) {
    pidNdx = StDedxPidTraits(kTpcId, kOtherMethodId, picoTrack->nHitsDedx(),
			     picoTrack->dNdx(), picoTrack->dNdxError());
    fdNdx = new StdEdxStatus(&pidNdx);
  }
  Int_t ibtof = picoTrack->bTofPidTraitsIndex();
  if (ibtof >= 0) {
    static StBTofPidTraits pidBTof;
    pidBTof = SetBTofPidTraits(*StPicoDst::instance()->btofPidTraits(ibtof));
    fBTof = new StBTofStatus(&pidBTof);
  }
  Int_t ietof = picoTrack->eTofPidTraitsIndex();
  if (ietof >= 0) {
    static StETofPidTraits pidETof;
    pidETof = SetETofPidTraits(*StPicoDst::instance()->etofPidTraits(ietof));
    fETof = new StETofStatus(&pidETof);
  }
  Int_t imtd = picoTrack->mtdPidTraitsIndex();
  if (imtd >= 0) {
    static StMtdPidTraits pidMtd;
    pidMtd = SetMtdPidTraits(*StPicoDst::instance()->mtdPidTraits(imtd));
    fMtd = new StMtdStatus(&pidMtd);
  }

  Set();
}
#endif /* __TFG__VERSION__ */
//________________________________________________________________________________
void StPidStatus::Set() {
  if (! fI70 && ! fFit && ! fdNdx) return;
  PiDStatus = 0;
  Double_t pMomentum = g3.mag();
  //  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Int_t l;
  PredBMN[0] = Pred70BMN[0] =  1;
  PredBMN[1] = Pred70BMN[1] = -1;
  memset (fStatus, 0, sizeof(fStatus));
  fStatus[kTruncatedMeanId]         = fI70   ;
  fStatus[kLikelihoodFitId]         = fFit   ;
  fStatus[kEnsembleTruncatedMeanId] = fI70U  ;
  fStatus[kWeightedTruncatedMeanId] = fFitU  ;
  fStatus[kOtherMethodId]           = fdNdx  ;
  fStatus[kOtherMethodId2]          = fdNdxU ;
  for (l = kPidElectron; l < KPidParticles; l++) {
    Int_t charge = StProbPidTraits::mPidParticleDefinitions[l]->charge();
    Double_t mass   = StProbPidTraits::mPidParticleDefinitions[l]->mass();
    Double_t betagamma = pMomentum*TMath::Abs(charge)/mass;
    bgs[l] = betagamma;
    bghyp[l] = TMath::Log10(bgs[l]);
    for (Int_t k = 1; k <= kOtherMethodId2; k++) {
      if (! fStatus[k]) continue;
      UChar_t fit = 0;
      if (k == kLikelihoodFitId || k == kWeightedTruncatedMeanId) fit = 1;
      else if (k == kOtherMethodId || k == kOtherMethodId2) fit = 2;
      if (fUsedx2) 
	fStatus[k]->Pred[l] = StdEdxPull::EvalPred2(betagamma, fStatus[k]->log2dX(), fit, charge);
      else 
	fStatus[k]->Pred[l] = StdEdxPull::EvalPred(betagamma, fit, charge);
      if (fStatus[k]->I() > 0) {
	fStatus[k]->dev[l] = TMath::Log(fStatus[k]->I()/fStatus[k]->Pred[l]);
	fStatus[k]->devS[l] = -999;
	if (fStatus[k]->D() > 0) {
	  fStatus[k]->devS[l] = fStatus[k]->dev[l]/fStatus[k]->D();
	}
      }
      if (fStatus[k]->Pred[l] < PredBMN[0]) PredBMN[0] = fStatus[k]->Pred[l];
      if (fStatus[k]->Pred[l] > PredBMN[1]) PredBMN[1] = fStatus[k]->Pred[l];
    }
  }
  PiDkey    = -1; // best
  PiDkeyU   = -1; // only one with devZs<3, 
  PiDkeyU3  = -1; // -"- and devZs > 5 for all others 
  lBest     = -1;
#if 0
  Int_t lBestTof = -1;
  // use Tof 
  Double_t devZkin = 999;
  if (fBTof->fPiD) {
    for (l = kPidElectron; l < KPidParticles; l++) {
      if      (l == kPidMuon    ) devTof[l] = devTof[kPidPion];
      else if (l >= kPidDeuteron) devTof[l] = devTof[kPidProton];
      else                        devTof[l] = TMath::Abs(fBTof.Sigma(l));
    }
    for (l = kPidElectron; l < KPidParticles; l++) {
      if (devTof[l] < 3.0) PiDStatus |= 1<<l;
      if (devTof[l] < devZmin) {devZmin = devTof[l]; lBestTof = l;}
    }
    if (devZmin > 5) {lBestTof = -1;}
  }
  devZmin = 999;
  for (l = kPidElectron; l < KPidParticles; l++) {
    if (devZs[l] < 3.0) PiDStatus |= 1<<l;
    Double_t dev = devZs[l]*devZs[l];
    if (lBestTof >= 0) dev += devTof[l]*devTof[l];
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
	if (lBestTof >= 0) dev += devTof[l]*devTof[l];
	if (dev < devZmin) {devZmin = dev; lNext = l;}
      }
      if (lNext >= 0) {
	Double_t dev = devZs[lNext]*devZs[lNext];
	if (lBestTof >= 0) dev += devTof[lNext]*devTof[lNext];
	if (dev > 9.) {
	  PiDkeyU = PiDkey;
	  if (dev > 25.) PiDkeyU3 = PiDkeyU;
	}
      }
    }
  }
#endif
}
//________________________________________________________________________________
void StPidStatus::Print(Option_t *opt) const {
  for (Int_t k = 1; k <= kOtherMethodId2; k++) {
    if (! fStatus[k]) continue;
    if      (k == kUndefinedMethodId)         cout << "UndefinedMethod       ";
    else if (k == kTruncatedMeanId)           cout << "TruncatedMean         ";
    else if (k == kEnsembleTruncatedMeanId)   cout << "EnsembleTruncatedMean ";
    else if (k == kLikelihoodFitId)           cout << "LikelihoodFit         ";
    else if (k == kWeightedTruncatedMeanId)   cout << "WeightedTruncatedMean ";
    else if (k == kOtherMethodId)             cout << "OtherMethod           "; 
    else if (k == kOtherMethodId2)            cout << "OtherMethodIdentifier2";
    fStatus[k]->Print();
  }
}
//________________________________________________________________________________
void StdEdxStatus::Print(Option_t *option) const {
  if (! fPiD) {cout << "\tEmpty" << endl;}
  else {
    Double_t scale = 1;
    if (I() < 10) cout << "\tI = " << scale*I() << "keV";
    else          cout << "\tI = " << I();
    cout << " +/- " << 100*D() << "%\tPred: ";
    for (Int_t l = kPidElectron; l <= kPidPion; l++) {cout << "\t" << scale*Pred[l];}
    cout << "\tdev:";
    for (Int_t l = kPidElectron; l <= kPidPion; l++) {cout << "\t" << dev[l];}
    cout << "\tdevS:";
    for (Int_t l = kPidElectron; l <= kPidPion; l++) {cout << "\t" << devS[l];}
    cout << endl;
  }
}
