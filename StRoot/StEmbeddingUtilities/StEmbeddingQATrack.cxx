/****************************************************************************************************
 * $Id: StEmbeddingQATrack.cxx,v 1.16 2011/02/11 03:55:50 hmasui Exp $
 * $Log: StEmbeddingQATrack.cxx,v $
 * Revision 1.16  2011/02/11 03:55:50  hmasui
 * Change geantid type to integer
 *
 * Revision 1.15  2011/02/09 20:56:07  hmasui
 * Fix initialization of particle id's for real data
 *
 * Revision 1.14  2011/01/12 21:36:15  hmasui
 * Add nHitsFit/nHitsPoss cut
 *
 * Revision 1.13  2010/08/13 21:55:36  hmasui
 * Separate charge for pi/K/p in isNSigmaOk() function
 *
 * Revision 1.12  2010/08/04 21:16:50  hmasui
 * Use MC phi angle for reconstructed embedding tracks
 *
 * Revision 1.11  2010/07/12 21:28:55  hmasui
 * Use StEmbeddingQAUtilities::getParticleDefinition() instead of StParticleTable
 *
 * Revision 1.10  2010/05/14 19:49:09  hmasui
 * Add rapidity cut
 *
 * Revision 1.9  2010/04/24 20:20:15  hmasui
 * Add geant process, and modift the type of parent, parent-parent geantid to match the data members in minimc tree
 *
 * Revision 1.8  2010/02/16 02:11:49  hmasui
 * Add parent-parent geantid
 *
 * Revision 1.7  2010/02/12 16:24:54  hmasui
 * Fix the primary vertex subtraction for nhit
 *
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
#include "StParticleDefinition.hh"

#include "StEmbeddingQATrack.h"
#include "StEmbeddingQAUtilities.h"

using namespace std ;

  /// Define static const data members here
  const Float_t StEmbeddingQATrack::kPtMinCut   = 0.1 ;   /// Minimum pt cut, pt > 0.1 GeV/c
  const Float_t StEmbeddingQATrack::kPtMaxCut   = 10.0 ;  /// Minimum pt cut, pt < 10 GeV/c
  const Float_t StEmbeddingQATrack::kEtaCut     = 1.5 ;   /// Eta cut, |eta| < 1.5
  const Short_t StEmbeddingQATrack::kNHitCut    = 10 ;    /// Minimum Nfit cut, Nfit >= 10
  const Float_t StEmbeddingQATrack::kNHitToNPossCut = 0.51 ;    /// Minimum Nfit cut, NHitFit/NHitPoss > 0.51
  const Float_t StEmbeddingQATrack::kDcaCut     = 3.0 ;   /// Global dca cut, |dca_{gl}| < 3 cm
  const Double_t StEmbeddingQATrack::kNSigmaCut = 2.0 ;   /// Nsigma cut, |Nsigma| < 2

ClassImp(StEmbeddingQATrack)

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack()
  : mNCommonHit(-10), mParentParentGeantId(-10), mParentGeantId(-10), mGeantId(-10), mGeantProcess(-10),
  mNHit(-10), mNHitPoss(-10), mCharge(-10),
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
  : mNCommonHit(-10), 
  mParentParentGeantId(-10), mParentGeantId(track.parentGeantId()), mGeantId(track.geantId()), mGeantProcess(-10),
  mNHit(track.nHitMc()), mNHitPoss(-10), mCharge(track.chargeMc()),
  mVectorMc(track.pxMc(), track.pyMc(), track.pzMc(), 
      TMath::Sqrt(track.pMc()*track.pMc() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track.geantId())->mass(),2.0))),
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
  : mNCommonHit(track->commonHits()),
  mParentParentGeantId(-10), mParentGeantId(track->parentGeantId()), mGeantId(track->geantId()), mGeantProcess(-10),
  mNHit(track->fitPts()), mNHitPoss(track->nPossiblePts()), mCharge(track->charge()),
  mVectorMc(track->pxMc(), track->pyMc(), track->pzMc(), 
      TMath::Sqrt(track->pMc()*track->pMc() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track->geantId())->mass(),2.0))),
  mVectorRc(track->pxPr(), track->pyPr(), track->pzPr(), 
      TMath::Sqrt(track->pPr()*track->pPr() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track->geantId())->mass(),2.0))),
  mPhi(track->phiMc()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName(name)
{
  /// Constructor for Matched or Mathced global pairs (StMiniMcPair)
  /// No Nsigma informations
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, StContamPair* track)
  : mNCommonHit(track->commonHits()), 
  mParentParentGeantId(track->mParentParentGeantId), mParentGeantId(track->parentGeantId()), mGeantId(track->geantId()), 
  mGeantProcess(track->mGeantProcess),
  mNHit(track->fitPts()), mNHitPoss(track->nPossiblePts()), mCharge(track->charge()),
  mVectorMc(track->pxMc(), track->pyMc(), track->pzMc(), 
      TMath::Sqrt(track->pMc()*track->pMc() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track->geantId())->mass(),2.0))),
  mVectorRc(track->pxPr(), track->pyPr(), track->pzPr(), 
      TMath::Sqrt(track->pPr()*track->pPr() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track->geantId())->mass(),2.0))),
  mPhi(track->phiMc()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName(name)
{
  /// Constructor for Contaminated pairs (StContamPair)
  /// No Nsigma informations
  /// Implement parent-parent geantid 
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, const StMuTrack& track, const Int_t geantid)
  : mNCommonHit(0), mParentParentGeantId(0), mParentGeantId(0), mGeantId(geantid), mGeantProcess(0),
  mNHit(track.nHitsFit(kTpcId)), mNHitPoss(track.nHitsPoss(kTpcId)), mCharge(track.charge()),
  mVectorMc(-9999., -9999., -9999., -9999.), // No MC momentum for real tracks
  mVectorRc(track.p().x(), track.p().y(), track.p().z(), 
      TMath::Sqrt(track.p().mag2() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(geantid)->mass(),2.0))),
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
Bool_t StEmbeddingQATrack::isRapidityOk(const Double_t ycut) const
{
  /// Rapidity cut (mainly for real data)
  /// No cut on the MC tracks
  const Float_t y = (StEmbeddingQAUtilities::instance()->isReal(mName)) ? getRapidityRc() : getRapidityMc() ;

  return (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE : (TMath::Abs(y)<ycut) ;
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
Bool_t StEmbeddingQATrack::isNHitToNPossOk() const
{
  /// Cut ratio of NHitFit to NHitPoss
  const Float_t ratio = (mNHitPoss>0) ? (Float_t)mNHit/(Float_t)mNHitPoss : -1.0 ;

  return (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE : ratio > kNHitToNPossCut ;
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
Bool_t StEmbeddingQATrack::isNSigmaOk(const Int_t geantid) const
{
  /// Nsigma cut for electrons/pions/kaons/protons

  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  /// No NSigma cut for embedding tracks
  if ( !utility->isReal(mName) ) return kTRUE ;

  /// NSigma cut will only apply for e/pi/K/p
  if ( !utility->isEPiKP(geantid) ) return kTRUE ;

  /// NSigma cut for e, pi, K and p
  /// Implement different charge
  if( mCharge < 0 ){
    // Negative charged particles
    if ( utility->isElectron(geantid) )     return TMath::Abs(mNSigmaElectron) < kNSigmaCut ;
    else if ( utility->isPiMinus(geantid) ) return TMath::Abs(mNSigmaPion) < kNSigmaCut ;
    else if ( utility->isKMinus(geantid) )  return TMath::Abs(mNSigmaKaon) < kNSigmaCut ;
    else if ( utility->isPBar(geantid) )    return TMath::Abs(mNSigmaProton) < kNSigmaCut ;
    else{
      return kTRUE ;
    }
  }
  else{
    // Positive charged particles
    if ( utility->isPositron(geantid) )    return TMath::Abs(mNSigmaElectron) < kNSigmaCut ;
    else if ( utility->isPiPlus(geantid) ) return TMath::Abs(mNSigmaPion) < kNSigmaCut ;
    else if ( utility->isKPlus(geantid) )  return TMath::Abs(mNSigmaKaon) < kNSigmaCut ;
    else if ( utility->isProton(geantid) ) return TMath::Abs(mNSigmaProton) < kNSigmaCut ;
    else{
      return kTRUE ;
    }
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
  LOG_INFO << "  getNCommonHit()           " <<  getNCommonHit()  << endm;
  LOG_INFO << "  getParentParentGeantId()  " <<  getParentParentGeantId()     << endm;
  LOG_INFO << "  getParentGeantId()        " <<  getParentGeantId()     << endm;
  LOG_INFO << "  getGeantId()              " <<  getGeantId()     << endm;
  LOG_INFO << "  getGeantProcess()         " <<  getGeantProcess()     << endm;
  LOG_INFO << "  getNHit()                 " <<  getNHit()        << endm;
  LOG_INFO << "  getMass() (MC, RC)       (" <<  getMassMc() << ", " << getMassRc() << ")" << endm;
  LOG_INFO << "  getPt() (MC, RC)         (" <<  getPtMc() << ", " << getPtRc() << ")" << endm;
  LOG_INFO << "  getPx() (MC, RC)         (" <<  getPxMc() << ", " << getPxRc() << ")" << endm;
  LOG_INFO << "  getPy() (MC, RC)         (" <<  getPyMc() << ", " << getPyRc() << ")" << endm;
  LOG_INFO << "  getPz() (MC, RC)         (" <<  getPzMc() << ", " << getPzRc() << ")" << endm;
  LOG_INFO << "  getP()  (MC, RC)         (" <<  getPMc() << ", " << getPRc() << ")" << endm;
  LOG_INFO << "  getEta() (MC, RC)        (" <<  getEtaMc() << ", " << getEtaRc() << ")" << endm;
  LOG_INFO << "  getPhi()                  " <<  getPhi()         << endm;
  LOG_INFO << "  getdEdx()                 " <<  getdEdx()        << endm;
  LOG_INFO << "  getDcaGl()                " <<  getDcaGl()       << endm;
  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
}


