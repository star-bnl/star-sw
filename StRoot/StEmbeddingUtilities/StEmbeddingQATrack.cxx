/****************************************************************************************************
 * $Id: StEmbeddingQATrack.cxx,v 1.6 2009/12/22 21:39:30 hmasui Exp $
 * $Log: StEmbeddingQATrack.cxx,v $
 * Revision 1.6  2009/12/22 21:39:30  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/

#include <iostream>

#include "TMath.h"

#include "StMessMgr.h"
#include "StMiniMcEvent/StTinyMcTrack.h"
#include "StMiniMcEvent/StMiniMcPair.h"
#include "StMiniMcEvent/StContamPair.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

#include "StEmbeddingQATrack.h"
#include "StEmbeddingQAUtilities.h"

using namespace std ;

  /// Define static const data members here
  const Float_t StEmbeddingQATrack::kPtMinCut   = 0.1 ;   /// Minimum pt cut, pt > 0.1 GeV/c
  const Float_t StEmbeddingQATrack::kPtMaxCut   = 10.0 ;  /// Minimum pt cut, pt < 10 GeV/c
  const Float_t StEmbeddingQATrack::kEtaCut     = 1.5 ;   /// Eta cut, |eta| < 1.5
  const Short_t StEmbeddingQATrack::kNHitCut    = 10 ;    /// Minimum Nfit cut, Nfit >= 10
  const Float_t StEmbeddingQATrack::kDcaCut     = 3.0 ;   /// Global dca cut, |dca_{gl}| < 3 cm
  const Double_t StEmbeddingQATrack::kNSigmaCut = 2.0 ;   /// Nsigma cut, |Nsigma| < 2

ClassImp(StEmbeddingQATrack)

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack()
  : mNCommonHit(-10), mParentGeantId(-10), mGeantId(-10), mNHit(-10), mNHitPoss(-10), mCharge(-10),
  mVectorMc(-9999., -9999., -9999., -9999.),
  mVectorRc(-9999., -9999., -9999., -9999.),
  mPhi(-9999.), mdEdx(-9999.), mDcaGl(-9999.), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName("MC")
{
  /// Default constructor
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, const StTinyMcTrack& track)
  : mNCommonHit(-10), mParentGeantId(track.parentGeantId()), mGeantId(track.geantId()), 
  mNHit(track.nHitMc()), mNHitPoss(-10), mCharge(track.chargeMc()),
  mVectorMc(track.pxMc(), track.pyMc(), track.pzMc(), 
      TMath::Sqrt(track.pMc()*track.pMc() + TMath::Power(StParticleTable::instance()->findParticleByGeantId(track.geantId())->mass(),2.0))),
  mVectorRc(-9999., -9999., -9999., -9999.), // No reconstructed momentum for MC tracks
  mPhi(track.phiMc()), mdEdx(-9999.), mDcaGl(-9999.), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName(name)
{
  /// Constructor for embedding MC tracks (StTinyMcTrack)
  /// No NcommontHit, maximum number of fit points, reconstructed momentum, dEdx, global dca and Nsigma informations
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, StMiniMcPair* track)
  : mNCommonHit(track->commonHits()), mParentGeantId(track->parentGeantId()), mGeantId(track->geantId()), 
  mNHit(track->fitPts()), mNHitPoss(track->nPossiblePts()), mCharge(track->charge()),
  mVectorMc(track->pxMc(), track->pyMc(), track->pzMc(), 
      TMath::Sqrt(track->pMc()*track->pMc() + TMath::Power(StParticleTable::instance()->findParticleByGeantId(track->geantId())->mass(),2.0))),
  mVectorRc(track->pxPr(), track->pyPr(), track->pzPr(), 
      TMath::Sqrt(track->pPr()*track->pPr() + TMath::Power(StParticleTable::instance()->findParticleByGeantId(track->geantId())->mass(),2.0))),
  mPhi(track->phiPr()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName(name)
{
  /// Constructor for Matched or Mathced global pairs (StMiniMcPair)
  /// No Nsigma informations
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, StContamPair* track)
  : mNCommonHit(track->commonHits()), mParentGeantId(track->parentGeantId()), mGeantId(track->geantId()), 
  mNHit(track->fitPts()), mNHitPoss(track->nPossiblePts()), mCharge(track->charge()),
  mVectorMc(track->pxMc(), track->pyMc(), track->pzMc(), 
      TMath::Sqrt(track->pMc()*track->pMc() + TMath::Power(StParticleTable::instance()->findParticleByGeantId(track->geantId())->mass(),2.0))),
  mVectorRc(track->pxPr(), track->pyPr(), track->pzPr(), 
      TMath::Sqrt(track->pPr()*track->pPr() + TMath::Power(StParticleTable::instance()->findParticleByGeantId(track->geantId())->mass(),2.0))),
  mPhi(track->phiGl()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName(name)
{
  /// Constructor for Contaminated pairs (StContamPair)
  /// No Nsigma informations
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, const StMuTrack& track, const Short_t geantid)
  : mNCommonHit(-10), mParentGeantId(-10), mGeantId(geantid),
  mNHit(track.nHitsFit(kTpcId)-1), mNHitPoss(track.nHitsPoss(kTpcId)-1), mCharge(track.charge()),
  mVectorMc(-9999., -9999., -9999., -9999.), // No MC momentum for real tracks
  mVectorRc(track.p().x(), track.p().y(), track.p().z(), 
      TMath::Sqrt(track.p().mag2() + TMath::Power(StParticleTable::instance()->findParticleByGeantId(geantid)->mass(),2.0))),
  mPhi(track.phi()), mdEdx(track.dEdx()), mDcaGl(track.dcaGlobal().mag()), 
  mNSigmaElectron(track.nSigmaElectron()), mNSigmaPion(track.nSigmaPion()), mNSigmaKaon(track.nSigmaKaon()), mNSigmaProton(track.nSigmaProton()),
  mName(name)
{
  /// Constructor for real data track (StMuTrack)
  /// No NcommonHit, parent geantid and MC momentum informations
  /// NOTE: subtract primary vertex from number of fit points in order to compare the nfit with the embedding tracks

  // Sep/21/2009
  // - Get NHit only from TPC in order to avoid the SVT/SSD hit, and remove primary vertex from the NHit
  // - Get nSigma(pi/K/p) for real data
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::~StEmbeddingQATrack()
{
  /// Destructor
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isPtAndEtaOk() const
{ 
  /// Pt cut only for MC tracks. 
  /// Pt & eta cuts for embedding/real tracks
  const Float_t pt     = (StEmbeddingQAUtilities::instance()->isReal(mName)) ? getPtRc() : getPtMc() ;
  const Float_t eta    = (StEmbeddingQAUtilities::instance()->isReal(mName)) ? getEtaRc() : getEtaMc() ;

  const Bool_t isPtOk  = pt > kPtMinCut ;
  const Bool_t isEtaOk = TMath::Abs(eta) < kEtaCut ;

  return (StEmbeddingQAUtilities::instance()->isMc(mName)) ? isPtOk : (isPtOk && isEtaOk) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isNHitOk() const
{ 
  /// Add NcommonHit cuts for embedding tracks (see isCommonHitOk())
  /// No NHit cut for MC tracks
  const Bool_t isNHitOk = (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE : mNHit >= kNHitCut ;

  return isCommonHitOk() && isNHitOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isDcaOk() const
{ 
  /// Dca cut
  /// No Dca cut for MC tracks

  return (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE : (mDcaGl >= 0.0 && mDcaGl < kDcaCut) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isCommonHitOk() const
{ 
  /// NcommonHit cuts (only for embedding tracks)

  return (StEmbeddingQAUtilities::instance()->isEmbedding(mName)) ? mNCommonHit >= kNHitCut : kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isNSigmaOk(const Short_t geantid) const
{
  /// Nsigma cut for electrons/pions/kaons/protons

  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  /// No NSigma cut for embedding tracks
  if ( !utility->isReal(mName) ) return kTRUE ;

  /// NSigma cut will only apply for e/pi/K/p
  if ( !utility->isEPiKP(geantid) ) return kTRUE ;

  /// NSigma cut for e, pi, K and p
  if ( utility->isElectrons(geantid) )    return TMath::Abs(mNSigmaElectron) < kNSigmaCut ;
  else if ( utility->isPions(geantid) )   return TMath::Abs(mNSigmaPion) < kNSigmaCut ;
  else if ( utility->isKaons(geantid) )   return TMath::Abs(mNSigmaKaon) < kNSigmaCut ;
  else if ( utility->isProtons(geantid) ) return TMath::Abs(mNSigmaProton) < kNSigmaCut ;
  else{
    return kTRUE ;
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
StLorentzVectorD StEmbeddingQATrack::getVectorMc() const
{ 
  /// Get MC 4-momentum vector

  return mVectorMc ;
}

//____________________________________________________________________________________________________
StLorentzVectorD StEmbeddingQATrack::getVectorRc() const
{ 
  /// Get reconstructed 4-momentum vector

  return mVectorRc ;
}

//____________________________________________________________________________________________________
void StEmbeddingQATrack::print() const
{
  /// Print track informations

  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
  LOG_INFO << Form("StEmbeddingQATrack::print() : Track informations (%s)", mName.Data()) << endm;
  LOG_INFO << "  getNCommonHit()      " <<  getNCommonHit()  << endm;
  LOG_INFO << "  getGeantId()         " <<  getGeantId()     << endm;
  LOG_INFO << "  getNHit()            " <<  getNHit()        << endm;
  LOG_INFO << "  getMass() (MC, RC)  (" <<  getMassMc() << ", " << getMassRc() << ")" << endm;
  LOG_INFO << "  getPt() (MC, RC)    (" <<  getPtMc() << ", " << getPtRc() << ")" << endm;
  LOG_INFO << "  getPx() (MC, RC)    (" <<  getPxMc() << ", " << getPxRc() << ")" << endm;
  LOG_INFO << "  getPy() (MC, RC)    (" <<  getPyMc() << ", " << getPyRc() << ")" << endm;
  LOG_INFO << "  getPz() (MC, RC)    (" <<  getPzMc() << ", " << getPzRc() << ")" << endm;
  LOG_INFO << "  getP()  (MC, RC)    (" <<  getPMc() << ", " << getPRc() << ")" << endm;
  LOG_INFO << "  getEta() (MC, RC)   (" <<  getEtaMc() << ", " << getEtaRc() << ")" << endm;
  LOG_INFO << "  getPhi()             " <<  getPhi()         << endm;
  LOG_INFO << "  getdEdx()            " <<  getdEdx()        << endm;
  LOG_INFO << "  getDcaGl()           " <<  getDcaGl()       << endm;
  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
}


