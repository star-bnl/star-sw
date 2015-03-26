#include "StPicoTrack.h"
#include "StPicoCut.h"
#include "StPicoConstants.h"
#include "StPicoDstMaker.h"
#include "TVector2.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

ClassImp(StPicoTrack)

//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack()
{
  Clear();
}

/////////////////////////////////////////////////////////////////////////////////////////
// t - the global track.  p - the associated primary track from the first primary vertex
/////////////////////////////////////////////////////////////////////////////////////////
//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack(StMuTrack* t, StMuTrack* p, float phi_weight, int flowFlag, double B, StDcaGeometry* dcaG)
{
  Clear();
  if(!t || t->type()!=global || (p && ( p->type()!=primary || p->id()!=t->id() ) ) ) {
    LOG_WARN << " Bad StPicoTrack constructor ...... Check!" << endm;
  } else {
  mId        = (UShort_t)t->id();
  mChi2      = (t->chi2()*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(t->chi2()*1000.));
//  mGMomentum = t->helix().momentum(B*kilogauss);
  if(p) {
    mPMomentum = p->p();
//    mChi2Prob  = (p->chi2prob()*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(p->chi2prob()*1000.));
  } else {
    mPMomentum.set(0.,0.,0.);
//    mChi2Prob = Pico::USHORTMAX ;
  }
//  StThreeVectorF o = t->helix().origin();
//  mOriginX   = (fabs(o.x()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(o.x()*100.));
//  mOriginY   = (fabs(o.y()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(o.y()*100.));
//  mOriginZ   = (fabs(o.z()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(o.z()*100.));
  int q      = t->charge();
//  mGDca      = (t->dcaGlobal().mag()*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(t->dcaGlobal().mag()*1000.));
  mDedx      = (t->dEdx()*1e6*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(t->dEdx()*1e6*1000.));
  int flag = t->flag();
  if(flag/100<7) { // TPC tracks
    mNHitsFit  = (UChar_t)(t->nHitsFit(kTpcId)*q);
//    mNHitsMax  = (Char_t)(t->nHitsPoss(kTpcId));
  } else { // FTPC tracks
//    if(mGMomentum.pseudoRapidity()>0.) {
  if(t->helix().momentum(B*kilogauss).pseudoRapidity()>0.) {
      mNHitsFit  = (UChar_t)(t->nHitsFit(kFtpcWestId)*q);
//      mNHitsMax  = (Char_t)(t->nHitsPoss(kFtpcWestId));
    } else {
      mNHitsFit  = (UChar_t)(t->nHitsFit(kFtpcEastId)*q);
//      mNHitsMax  = (Char_t)(t->nHitsPoss(kFtpcEastId));
    }
  }
  mNHitsDedx = (Char_t)(t->nHitsDedx());
  mNSigmaPion     = (fabs(t->nSigmaPion()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaPion()*100.));
  mNSigmaKaon     = (fabs(t->nSigmaKaon()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaKaon()*100.));
  mNSigmaProton   = (fabs(t->nSigmaProton()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaProton()*100.));
  mNSigmaElectron = (fabs(t->nSigmaElectron()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaElectron()*100.));

  unsigned int map0 = t->topologyMap().data(0);
  unsigned int map1 = t->topologyMap().data(1);
  mNHitsMapHFT = map0>>1 & 0x7F;  // see hitMap definition in StTrackTopologyMap
  mFirstTpcHitRow = 0;
  mLastTpcHitRow = 0;
  for(int row=1;row<=24;row++) {
    if(map0>>(row+7) & 0x1) {
      mFirstTpcHitRow = row; break;      
    }
  }
  if(mFirstTpcHitRow==0) {
    for(int row=25;row<=45;row++) {
      if(map1>>(row-25) & 0x1) {
        mFirstTpcHitRow = row; break;
      }
    }
  }
  for(int row=45;row>=25;row--) {
    if(map1>>(row-25) & 0x1) {
      mLastTpcHitRow = row; break;
    }
  }
  if(mLastTpcHitRow==0) {
    for(int row=24;row>=1;row--) {         
      if(map0>>(row+7) & 0x1) {   
        mLastTpcHitRow = row; break;
      }
    }
  }

/*
  map0 &= 0xffffff00;
  map1 &= 0x001fffff;
  cout << " map = " << hex << map0 << "\t" << hex << map1 << endl;
  cout << "  first/last tpc hit row = " << dec << (Int_t)mFirstTpcHitRow << "\t" << dec << (Int_t)mLastTpcHitRow << endl;
*/

  // Flow analysis
//  mFlowFlag = (UChar_t)(flowFlag);

  if(dcaG) {
    const float* params = dcaG->params();
    const float* errMatrix = dcaG->errMatrix();
    for(int i=0;i<6;i++) mPar[i] = params[i];
    for(int i=0;i<15;i++) mErrMatrix[i] = errMatrix[i];

  } else {
    cout << " This track doesn't have a dcaGeometry!!!!" << endl;
  }

#if 0
  if(mFlowFlag==others) {
    mQXi = 0.;
    mQYi = 0.;
  } else {
    float pt = mPMomentum.perp();
    float pt_weight = (pt<2.) ? pt : 2.;
    mQXi = pt_weight * phi_weight * TMath::Cos(2.*mPMomentum.phi());
    mQYi = pt_weight * phi_weight * TMath::Sin(2.*mPMomentum.phi());
    if(mQXi > 10. || mQYi > 10.) {
      LOG_WARN << " WARN!!! something is wrong with this Qi " << mQXi << " " << mQYi << " flag=" << flowFlag << " phi_weight=" << phi_weight << " pt=" << pt << endm;
    }
  }
#endif

  // pid Traits
  mEmcPidTraitsIndex  = -1;
  mBTofPidTraitsIndex = -1;
  mMtdPidTraitsIndex  = -1;

  }// end if
}

//----------------------------------------------------------------------------------
StPicoTrack::~StPicoTrack()
{ /* noop */ }

//----------------------------------------------------------------------------------
void StPicoTrack::Clear(const Option_t* opt)
{
  mId = 0;
  mChi2 = Pico::USHORTMAX;
//  mChi2Prob = Pico::USHORTMAX;
//  mGMomentum.set(0.,0.,0.);
  mPMomentum.set(0.,0.,0.);
//  mFlowFlag = 0;
//  mQXi = 0.;
//  mQYi = 0.;
//  mOriginX = 0;
//  mOriginY = 0;
//  mOriginZ = 0;
//  mGDca = Pico::USHORTMAX;
  mDedx = 0;
  mNHitsFit  = 0;
//  mNHitsMax  = 0;
  mNHitsDedx = 0;
  mNHitsMapHFT = 0;
  mFirstTpcHitRow = 0;
  mLastTpcHitRow = 0;  
  mNSigmaPion     = Pico::SHORTMAX;
  mNSigmaKaon     = Pico::SHORTMAX;
  mNSigmaProton   = Pico::SHORTMAX;
  mNSigmaElectron = Pico::SHORTMAX;
  for(int i=0;i<6;i++) mPar[i] = 0;
  for(int i=0;i<15;i++) mErrMatrix[i] = 0;
  mEmcPidTraitsIndex = -1;
  mBTofPidTraitsIndex = -1;
  mMtdPidTraitsIndex = -1;  
}
//----------------------------------------------------------------------------------
void StPicoTrack::Print(const Char_t *option) const {
  if(strcmp(option,"tpc")==0 || strcmp(option,"")==0) {
    LOG_INFO << "id=" << id() 
// << " flowflag=" << flowFlag() 
             << " chi2=" << chi2() 
//             << " dca=" << dca() 
             << endm;
//    LOG_INFO << "gMom=" << gMom() << endm;
    LOG_INFO << "pMom=" << pMom() << endm;
//    LOG_INFO << "Origin=" << origin() << endm;
//    LOG_INFO << "Q vector=" << mQXi << " " << mQYi << endm;
    LOG_INFO << " nHitsFit = " << nHitsFit() << " nHitsdEdx = " << nHitsDedx() << endm;
    LOG_INFO << " nSigma Pi/K/P/E = " << nSigmaPion() << "/" << nSigmaKaon() << "/" << nSigmaProton() << "/" << nSigmaElectron() << endm;
  }
}
