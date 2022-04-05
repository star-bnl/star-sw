/****************************************************************************************************
 * $Id: StEmbeddingQATrack.cxx,v 1.19 2019/07/10 05:45:56 zhux Exp $
 * $Log: StEmbeddingQATrack.cxx,v $
 * Revision 1.19  2019/07/10 05:45:56  zhux
 * added option for btof pid for primary real tracks
 *
 * Revision 1.18  2011/04/01 05:00:20  hmasui
 * Move track cuts into StEmbeddingQAUtilities, added global momentum for embedding
 *
 * Revision 1.17  2011/02/11 23:22:12  hmasui
 * Added missing charge check in isNSigmaOk(), and error check for geantid
 *
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

#include <assert.h>
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
  mVectorGl(track->pxGl(), track->pyGl(), track->pzGl(), 
      TMath::Sqrt(track->pGl()*track->pGl() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track->geantId())->mass(),2.0))),
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
  mVectorGl(track->pxGl(), track->pyGl(), track->pzGl(), 
      TMath::Sqrt(track->pGl()*track->pGl() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(track->geantId())->mass(),2.0))),
  mPhi(track->phiMc()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), 
  mNSigmaElectron(-9999.), mNSigmaPion(-9999.), mNSigmaKaon(-9999.), mNSigmaProton(-9999.),
  mName(name)
{
  /// Constructor for Contaminated pairs (StContamPair)
  /// No Nsigma informations
  /// Implement parent-parent geantid 
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, const StMuTrack& track, const Int_t geantid, const Bool_t btof)
  : mNCommonHit(0), mParentParentGeantId(0), mParentGeantId(0), mGeantId(geantid), mGeantProcess(0),
  mNHit(track.nHitsFit(kTpcId)), mNHitPoss(track.nHitsPoss(kTpcId)), mCharge(track.charge()),
  mVectorMc(-9999., -9999., -9999., -9999.), // No MC momentum for real tracks
  mVectorRc(track.p().x(), track.p().y(), track.p().z(), 
      TMath::Sqrt(track.p().mag2() + TMath::Power(StEmbeddingQAUtilities::instance()->getParticleDefinition(geantid)->mass(),2.0))),
  mVectorGl(-9999., -9999., -9999., -9999.), // Global momentum will be filled in the mVectorRc for global tracks
  mPhi(track.phi()), mdEdx(track.dEdx()), mDcaGl(track.dcaGlobal().mag()), 
  mNSigmaElectron(btof?track.btofPidTraits().sigmaElectron():track.nSigmaElectron()), mNSigmaPion(btof?track.btofPidTraits().sigmaPion():track.nSigmaPion()), mNSigmaKaon(btof?track.btofPidTraits().sigmaKaon():track.nSigmaKaon()), mNSigmaProton(btof?track.btofPidTraits().sigmaProton():track.nSigmaProton()),
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
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Float_t pt     = (utility->isReal(mName)) ? getPtRc() : getPtMc() ;
  const Float_t eta    = (utility->isReal(mName)) ? getEtaRc() : getEtaMc() ;

  const Bool_t isPtOk  = utility->isPtOk(pt) ;
  const Bool_t isEtaOk = utility->isEtaOk(eta) ;

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
  const Bool_t isNHitOk = (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE
    : StEmbeddingQAUtilities::instance()->isNHitsFitOk(mNHit)
    ;

  return isCommonHitOk() && isNHitOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isNHitToNPossOk() const
{
  /// Cut ratio of NHitFit to NHitPoss
  const Float_t ratio = (mNHitPoss>0) ? (Float_t)mNHit/(Float_t)mNHitPoss : -1.0 ;

  return (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE
    : StEmbeddingQAUtilities::instance()->isNHitToNPossOk(ratio) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isDcaOk() const
{ 
  /// Dca cut
  /// No Dca cut for MC tracks

  return (StEmbeddingQAUtilities::instance()->isMc(mName)) ? kTRUE 
    : StEmbeddingQAUtilities::instance()->isDcaOk(mDcaGl) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::isCommonHitOk() const
{ 
  /// NcommonHit cuts (only for embedding tracks)
  // Use the 'isNHitsFitOk()' function for common hits

  return (StEmbeddingQAUtilities::instance()->isEmbedding(mName)) ? StEmbeddingQAUtilities::instance()->isNHitsFitOk(mNCommonHit)
    : kTRUE ;
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

  /// Check charge
  const Bool_t isChargeOk = utility->getParticleDefinition(geantid)->charge() == mCharge ;
  if(!isChargeOk) return kFALSE ;

  /// NSigma cut for e, pi, K and p
  /// Implement different charge
  if( mCharge < 0 ){
    // Negative charged particles
    if ( utility->isElectron(geantid) )     return utility->isNSigmaOk(mNSigmaElectron) ;
    else if ( utility->isPiMinus(geantid) ) return utility->isNSigmaOk(mNSigmaPion) ;
    else if ( utility->isKMinus(geantid) )  return utility->isNSigmaOk(mNSigmaKaon) ;
    else if ( utility->isPBar(geantid) )    return utility->isNSigmaOk(mNSigmaProton) ;
    else{
      /// This should not happen
      LOG_ERROR << "StEmbeddingQATrack::isNSigmaOk  Geant id is not e, pi, K or p, geantid= " << geantid << endm;
      LOG_ERROR << "StEmbeddingQATrack::isNSigmaOk  Please check geantid, real data QA should only contain e, pi, K and p" << endm;
      assert(0);
    }
  }
  else if ( mCharge > 0 ) {
    // Positive charged particles
    if ( utility->isPositron(geantid) )    return utility->isNSigmaOk(mNSigmaElectron) ;
    else if ( utility->isPiPlus(geantid) ) return utility->isNSigmaOk(mNSigmaPion) ;
    else if ( utility->isKPlus(geantid) )  return utility->isNSigmaOk(mNSigmaKaon) ;
    else if ( utility->isProton(geantid) ) return utility->isNSigmaOk(mNSigmaProton) ;
    else{
      /// This should not happen
      LOG_ERROR << "StEmbeddingQATrack::isNSigmaOk  Geant id is not e, pi, K or p, geantid= " << geantid << endm;
      LOG_ERROR << "StEmbeddingQATrack::isNSigmaOk  Please check geantid, real data QA should only contain e, pi, K and p" << endm;
      assert(0);
    }
  }
  else{
    /// This should not happen
    LOG_ERROR << "StEmbeddingQATrack::isNSigmaOk  Charge == 0, charge=" << mCharge << ", geantid= " << geantid << endm;
    LOG_ERROR << "StEmbeddingQATrack::isNSigmaOk  Please check geantid, real data QA should only contain e, pi, K and p" << endm;
    assert(0);
  }
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
  /// Get reconstructed 4-momentum vector (primary)

  return mVectorRc ;
}

//____________________________________________________________________________________________________
StLorentzVectorD StEmbeddingQATrack::getVectorPr() const
{ 
  /// Get reconstructed 4-momentum vector (primary)

  return getVectorRc() ;
}

//____________________________________________________________________________________________________
StLorentzVectorD StEmbeddingQATrack::getVectorGl() const
{ 
  /// Get reconstructed 4-momentum vector (global)

  return mVectorGl ;
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


