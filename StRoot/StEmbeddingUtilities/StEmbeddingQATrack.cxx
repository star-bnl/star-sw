
#include <iostream>

#include "TMath.h"

#include "StMiniMcEvent/StTinyMcTrack.h"
#include "StMiniMcEvent/StMiniMcPair.h"
#include "StMiniMcEvent/StContamPair.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StEmbeddingQATrack.h"

using namespace std ;

  const Float_t StEmbeddingQATrack::kPtMinCut  = 0.1 ;
  const Float_t StEmbeddingQATrack::kPtMaxCut  = 10.0 ;
  const Float_t StEmbeddingQATrack::kEtaCut    = 1.5 ;
  const Short_t StEmbeddingQATrack::kNHitCut   = 10 ;
  const Float_t StEmbeddingQATrack::kDcaCut    = 3.0 ;

ClassImp(StEmbeddingQATrack)

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack()
  : mNCommonHit(-10), mParentGeantId(-10), mGeantId(-10), mNHit(-10), mNHitPoss(-10), mCharge(-10),
  mVectorMc(-9999., -9999., -9999., -9999.),
  mVectorRc(-9999., -9999., -9999., -9999.),
  mPhi(-9999.), mdEdx(-9999.), mDcaGl(-9999.), mName("MC")
{
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, const StTinyMcTrack& track, const Double_t mass2)
  : mNCommonHit(-10), mParentGeantId(track.parentGeantId()), mGeantId(track.geantId()), 
  mNHit(track.nHitMc()), mNHitPoss(-10), mCharge(track.chargeMc()),
  mVectorMc(track.pxMc(), track.pyMc(), track.pzMc(), TMath::Sqrt(track.pMc()*track.pMc() + mass2)),
  mVectorRc(-9999., -9999., -9999., -9999.), // No reconstructed momentum for MC tracks
  mPhi(track.phiMc()), mdEdx(-9999.), mDcaGl(-9999.), mName(name)
{
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, StMiniMcPair* track, const Double_t mass2)
  : mNCommonHit(track->commonHits()), mParentGeantId(track->parentGeantId()), mGeantId(track->geantId()), 
  mNHit(track->fitPts()), mNHitPoss(track->nPossiblePts()), mCharge(track->charge()),
  mVectorMc(track->pxMc(), track->pyMc(), track->pzMc(), TMath::Sqrt(track->pMc()*track->pMc() + mass2)),
  mVectorRc(track->pxPr(), track->pyPr(), track->pzPr(), TMath::Sqrt(track->pPr()*track->pPr() + mass2)),
  mPhi(track->phiPr()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), mName(name)
{
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, StContamPair* track, const Double_t mass2)
  : mNCommonHit(track->commonHits()), mParentGeantId(track->parentGeantId()), mGeantId(track->geantId()), 
  mNHit(track->fitPts()), mNHitPoss(track->nPossiblePts()), mCharge(track->charge()),
  mVectorMc(track->pxMc(), track->pyMc(), track->pzMc(), TMath::Sqrt(track->pMc()*track->pMc() + mass2)),
  mVectorRc(track->pxPr(), track->pyPr(), track->pzPr(), TMath::Sqrt(track->pPr()*track->pPr() + mass2)),
  mPhi(track->phiGl()), mdEdx(track->dedx()), mDcaGl(track->dcaGl()), mName(name)
{
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::StEmbeddingQATrack(const TString name, const StMuTrack& track, const Double_t mass2)
  : mNCommonHit(-10), mParentGeantId(-10), mGeantId(-10), 
  mNHit(track.nHitsFit()), mNHitPoss(track.nHitsPoss()), mCharge(track.charge()),
  mVectorMc(-9999., -9999., -9999., -9999.), // No MC momentum for real tracks
  mVectorRc(track.p().x(), track.p().y(), track.p().z(), TMath::Sqrt(track.p().mag2() + mass2)),
  mPhi(track.phi()), mdEdx(track.dEdx()), mDcaGl(track.dcaGlobal().mag()), mName(name)
{
}

//____________________________________________________________________________________________________
StEmbeddingQATrack::~StEmbeddingQATrack()
{
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::IsMc() const
{
  return mName.Contains("MC") ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::IsEmbedding() const
{
  return mName.Contains("MATCHED") || mName.Contains("GHOST") || mName.Contains("CONTAM") ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::IsPtAndEtaOk() const
{ 
  // Pt cut for MC tracks, pt & eta cuts for embedding/real tracks

  const Bool_t isPtOk  = (IsMc()) ? GetPtMc() > kPtMinCut : GetPtRc() > kPtMinCut ;
  const Bool_t isEtaOk = TMath::Abs(GetEtaRc()) < kEtaCut ;

  return (IsMc()) ? isPtOk : (isPtOk && isEtaOk) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::IsNHitOk() const
{ 
  // Add NcommonHit cuts for embedding tracks (see IsCommonHitOk())
  // No NHit cut for MC tracks
  const Bool_t isNHitOk = (IsMc()) ? kTRUE : mNHit >= kNHitCut ;

  return IsCommonHitOk() && isNHitOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::IsDcaOk() const
{ 
  // No Dca cut for MC tracks

  return (IsMc()) ? kTRUE : (mDcaGl >= 0.0 && mDcaGl < kDcaCut) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQATrack::IsCommonHitOk() const
{ 
  // Only for embedding tracks

  return (IsEmbedding()) ? mNCommonHit >= kNHitCut : kTRUE ;
}

//____________________________________________________________________________________________________
StLorentzVectorD StEmbeddingQATrack::GetVectorMc() const
{ 
  return mVectorMc ;
}

//____________________________________________________________________________________________________
StLorentzVectorD StEmbeddingQATrack::GetVectorRc() const
{ 
  return mVectorRc ;
}

//____________________________________________________________________________________________________
void StEmbeddingQATrack::Print() const
{
  cout << "#----------------------------------------------------------------------------------------------------" << endl;
  cout << Form("StEmbeddingQATrack::Print() : Track informations (%s)", mName.Data()) << endl;
  cout << "  GetNCommonHit()      " <<  GetNCommonHit()  << endl;
  cout << "  GetGeantId()         " <<  GetGeantId()     << endl;
  cout << "  GetNHit()            " <<  GetNHit()        << endl;
  cout << "  GetMass() (MC, RC)  (" <<  GetMassMc() << ", " << GetMassRc() << ")" << endl;
  cout << "  GetPt() (MC, RC)    (" <<  GetPtMc() << ", " << GetPtRc() << ")" << endl;
  cout << "  GetPx() (MC, RC)    (" <<  GetPxMc() << ", " << GetPxRc() << ")" << endl;
  cout << "  GetPy() (MC, RC)    (" <<  GetPyMc() << ", " << GetPyRc() << ")" << endl;
  cout << "  GetPz() (MC, RC)    (" <<  GetPzMc() << ", " << GetPzRc() << ")" << endl;
  cout << "  GetP()  (MC, RC)    (" <<  GetPMc() << ", " << GetPRc() << ")" << endl;
  cout << "  GetEta() (MC, RC)   (" <<  GetEtaMc() << ", " << GetEtaRc() << ")" << endl;
  cout << "  GetPhi()             " <<  GetPhi()         << endl;
  cout << "  GetdEdx()            " <<  GetdEdx()        << endl;
  cout << "  GetDcaGl()           " <<  GetDcaGl()       << endl;
  cout << "#----------------------------------------------------------------------------------------------------" << endl;
}


