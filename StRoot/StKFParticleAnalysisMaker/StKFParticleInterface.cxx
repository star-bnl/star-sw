#include "StKFParticleInterface.h"

#include "KFParticleTopoReconstructor.h"
#include "KFMCTrack.h"

#include "TMath.h"
#include "TArrayD.h"
#include "TH1F.h"
#include "TH2F.h"

#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoHelix.h"
#include "StPicoEvent/StPicoPhysicalHelix.h"
#include "StPicoEvent/StPicoArrays.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoEvent/StPicoETofPidTraits.h"
#include "StPicoEvent/StPicoTrackCovMatrix.h"

#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StProbPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include <ctime>
#include <algorithm>
#include <stdint.h>

#define USETOF
#define FXT

ClassImp(StKFParticleInterface);

StKFParticleInterface *StKFParticleInterface::fgStKFParticleInterface = 0;
StKFParticleInterface::StKFParticleInterface(): 
  fKFParticleTopoReconstructor(0), fParticles(0), fParticlesPdg(0), fNHftHits(0), fBeamSpot(),
  fCollectTrackHistograms(false), fCollectPIDHistograms(false), fCollectPVHistograms(false),
  fStrictTofPID(true), fCleanKaonsWitTof(true), fUseETof(false), fdEdXMode(1), fTriggerMode(false),
  fChiPrimaryCut(18.6f), fChiPrimaryCutFragments(0.f), fChiPrimaryMaxCut(2e4f), fCleanLowPVTrackEvents(false), fUseHFTTracksOnly(false)
{
  fKFParticleTopoReconstructor = new KFParticleTopoReconstructor();
  fgStKFParticleInterface = this;
  // set default cuts
  SetPrimaryProbCut(0.0001); // 0.01% to consider primary track as a secondary;
}

StKFParticleInterface::~StKFParticleInterface()
{  
  if(fKFParticleTopoReconstructor) delete fKFParticleTopoReconstructor;
  fgStKFParticleInterface = 0;
}

void StKFParticleInterface::SetField(float field) 
{ 
  if(fKFParticleTopoReconstructor)
    fKFParticleTopoReconstructor->SetField(field); 
}
void StKFParticleInterface::SetBeamLine(KFParticle& p)
{ 
  if(fKFParticleTopoReconstructor)
    fKFParticleTopoReconstructor->SetBeamLine(p);
}

void StKFParticleInterface::SetBeamSpot(const TString beamSpotMode)
{
  fBeamSpot.NDF() = -1;

  if(beamSpotMode == TString("2018_3AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.2173 * 0.2173;
    fBeamSpot.Covariance(1,1) = 0.1362 * 0.1362;
    fBeamSpot.Covariance(2,2) = 0.1674 * 0.1674;

    fBeamSpot.X() = -0.008f;
    fBeamSpot.Y() = -2.02f;
    fBeamSpot.Z() = 200.7f;
  }
  else if(beamSpotMode == TString("2019_3AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1632 * 0.1632;
    fBeamSpot.Covariance(1,1) = 0.1347 * 0.1347;
    fBeamSpot.Covariance(2,2) = 0.1908 * 0.1908;

    fBeamSpot.X() = -0.6577f;
    fBeamSpot.Y() = -1.986f;
    fBeamSpot.Z() = 199.8f;
  }
  else if(beamSpotMode == TString("2019_4AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1665 * 0.1665;
    fBeamSpot.Covariance(1,1) = 0.125 * 0.125;
    fBeamSpot.Covariance(2,2) = 0.2 * 0.2;

    fBeamSpot.X() = -0.427f;
    fBeamSpot.Y() = -1.994f;
    fBeamSpot.Z() = 199.9f;
  }
  else if(beamSpotMode == TString("2019_7AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.18 * 0.18;
    fBeamSpot.Covariance(1,1) = 0.122 * 0.122;
    fBeamSpot.Covariance(2,2) = 0.2449 * 0.2449;

    fBeamSpot.X() = -0.45f;
    fBeamSpot.Y() = -1.994f;
    fBeamSpot.Z() = 200.2f;
  }
  else if(beamSpotMode == TString("2019_31AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1549 * 0.1549;
    fBeamSpot.Covariance(1,1) = 0.1239 * 0.1239;
    fBeamSpot.Covariance(2,2) = 0.2762 * 0.2762;

    fBeamSpot.X() = -0.2851f;
    fBeamSpot.Y() = -1.978f;
    fBeamSpot.Z() = 200.3f;
  }
  else if(beamSpotMode == TString("2020_5AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1595 * 0.1595;
    fBeamSpot.Covariance(1,1) = 0.122 * 0.122;
    fBeamSpot.Covariance(2,2) = 0.1837 * 0.1837;

    fBeamSpot.X() = -0.3592f;
    fBeamSpot.Y() = -2.038f;
    fBeamSpot.Z() = 200.2f;

    fUseETof = true;
  }
  else if(beamSpotMode == TString("2020_7AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1869 * 0.1869;
    fBeamSpot.Covariance(1,1) = 0.1235 * 0.1235;
    fBeamSpot.Covariance(2,2) = 0.2276 * 0.2276;

    fBeamSpot.X() = -0.3125f;
    fBeamSpot.Y() = -2.045f;
    fBeamSpot.Z() = 200.3f;

    fUseETof = true;
  }
  else if(beamSpotMode == TString("2020_9AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.2395 * 0.2395;
    fBeamSpot.Covariance(1,1) = 0.1239 * 0.1239;
    fBeamSpot.Covariance(2,2) = 0.242 * 0.242;

    fBeamSpot.X() = -0.2838f;
    fBeamSpot.Y() = -2.042f;
    fBeamSpot.Z() = 200.3f;

    fUseETof = true;
  }
  else if(beamSpotMode == TString("2020_13AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1967 * 0.1967;
    fBeamSpot.Covariance(1,1) = 0.1285 * 0.1285;
    fBeamSpot.Covariance(2,2) = 0.2656 * 0.2656;

    fBeamSpot.X() = -0.2365f;
    fBeamSpot.Y() = -2.026f;
    fBeamSpot.Z() = 200.4f;

    fUseETof = true;
  }
  else if(beamSpotMode == TString("2020_19AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.2083 * 0.2083;
    fBeamSpot.Covariance(1,1) = 0.1216 * 0.1216;
    fBeamSpot.Covariance(2,2) = 0.255 * 0.255;

    fBeamSpot.X() = -0.1866f;
    fBeamSpot.Y() = -2.021f;
    fBeamSpot.Z() = 200.4f;

    fUseETof = true;
  }
  else if(beamSpotMode == TString("2020_31AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1636 * 0.1636;
    fBeamSpot.Covariance(1,1) = 0.1169 * 0.1169;
    fBeamSpot.Covariance(2,2) = 0.2701 * 0.2701;

    fBeamSpot.X() = -0.1827f;
    fBeamSpot.Y() = -2.014f;
    fBeamSpot.Z() = 200.4f;

    fUseETof = true;
  }
  else if(beamSpotMode == TString("2021_3AGeV")) {
    fBeamSpot.Covariance(0,0) = 0.1549 * 0.1549;
    fBeamSpot.Covariance(1,1) = 0.1041 * 0.1041;
    fBeamSpot.Covariance(2,2) = 0.1553 * 0.1553;

    fBeamSpot.X() = 0.4037f;
    fBeamSpot.Y() = -2.054f;
    fBeamSpot.Z() = 200.3f;

    // fUseETof = true;
  }
  else {
    fBeamSpot.Covariance(0,0) = 0.26f;
    fBeamSpot.Covariance(1,1) = 0.14f;
    fBeamSpot.Covariance(2,2) = 0.20f;

    fBeamSpot.X() = 0.f;
    fBeamSpot.Y() = -2.0f;
    fBeamSpot.Z() = 200.f;
  }
}

void StKFParticleInterface::InitParticles()
{ 
  fKFParticleTopoReconstructor->Init( fParticles, &fParticlesPdg, &fNHftHits );
  Int_t NPV =  fKFParticleTopoReconstructor->NPrimaryVertices();
  fKFParticleTopoReconstructor->GetKFParticleFinder()->Init(NPV);
  fKFParticleTopoReconstructor->FillPVIndices();
}

#ifdef __kfpAtFirstHit__
void StKFParticleInterface::InitParticlesAtFirstAndLastHit()
{ 
  KFPTrackVector ftracksAtFirstHit;
  KFPTrackVector ftracksAtLastHit;
  int nTracks = fParticlesAtLastHit.size();
  ftracksAtFirstHit.Resize(nTracks);
  ftracksAtLastHit.Resize(nTracks);
  for(int iTr=0; iTr<nTracks; iTr++)
  {  
    int trackPDG = fParticlesPdg[iTr];
    int npixelhits = fNHftHits[iTr];
    
    for(int iP=0; iP<6; iP++) 
    {
      ftracksAtFirstHit.SetParameter(fParticles[iTr].Parameters()[iP], iP, iTr);
      ftracksAtLastHit.SetParameter(fParticlesAtLastHit[iTr].Parameters()[iP], iP, iTr); 
    }
    for(int iC=0; iC<21; iC++) 
    {
      ftracksAtFirstHit.SetCovariance(fParticles[iTr].CovarianceMatrix()[iC], iC, iTr);
      ftracksAtLastHit.SetCovariance(fParticlesAtLastHit[iTr].CovarianceMatrix()[iC], iC, iTr); 
    }
    ftracksAtFirstHit.SetId(fParticles[iTr].Id(), iTr);
    ftracksAtFirstHit.SetPDG(trackPDG, iTr);
    ftracksAtFirstHit.SetQ(fParticles[iTr].Q(), iTr);
    ftracksAtFirstHit.SetPVIndex(-1, iTr);
    ftracksAtFirstHit.SetNPixelHits(npixelhits,iTr);
    
    ftracksAtLastHit.SetId(fParticlesAtLastHit[iTr].Id(), iTr);
    ftracksAtLastHit.SetPDG(trackPDG, iTr);
    ftracksAtLastHit.SetQ(fParticlesAtLastHit[iTr].Q(), iTr);
    ftracksAtLastHit.SetPVIndex(-1, iTr);
    ftracksAtLastHit.SetNPixelHits(npixelhits,iTr);
  }
  fKFParticleTopoReconstructor->Init( ftracksAtFirstHit, ftracksAtLastHit );
  Int_t NPV =  fKFParticleTopoReconstructor->NPrimaryVertices();
  fKFParticleTopoReconstructor->GetKFParticleFinder()->Init(NPV);
  fKFParticleTopoReconstructor->FillPVIndices();
}
#endif

void StKFParticleInterface::ReconstructParticles()
{ 
  fKFParticleTopoReconstructor->SortTracks();
  fKFParticleTopoReconstructor->ReconstructParticles();
 
}

void StKFParticleInterface::ReconstructTopology()
{ 
  fKFParticleTopoReconstructor->Init( fParticles, &fParticlesPdg );
  fKFParticleTopoReconstructor->ReconstructPrimVertex(0);
  fKFParticleTopoReconstructor->SortTracks();
  fKFParticleTopoReconstructor->ReconstructParticles();
}

void StKFParticleInterface::AddPV(const KFVertex &pv, const vector<int> &tracks) { 
  fKFParticleTopoReconstructor->AddPV(pv, tracks);
  fKFParticleTopoReconstructor->FillPVIndices();
}
void StKFParticleInterface::CleanPV() {
  fKFParticleTopoReconstructor->CleanPV();
}

void StKFParticleInterface::AddPV(const KFVertex &pv) { 
  fKFParticleTopoReconstructor->AddPV(pv);
}

void StKFParticleInterface::AddParticle(const KFParticle &p) { 
  fKFParticleTopoReconstructor->AddParticle(p);
}

void StKFParticleInterface::AddCandidate(const KFParticle& candidate, int iPV) {
  fKFParticleTopoReconstructor->AddCandidate(candidate, iPV);
}
void StKFParticleInterface::AddDecayToReconstructionList(Int_t pdg) {
   fKFParticleTopoReconstructor->GetKFParticleFinder()->AddDecayToReconstructionList(pdg);
}
std::vector<KFParticle> const &StKFParticleInterface::GetParticles() const { return fKFParticleTopoReconstructor->GetParticles(); }
void StKFParticleInterface::RemoveParticle(const int iParticle) { fKFParticleTopoReconstructor->RemoveParticle(iParticle); }
const std::vector<KFParticle>* StKFParticleInterface::GetSecondaryCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryCandidates();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryK0() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryK0();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryLambda() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryLambda();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryAntiLambda() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryAntiLambda();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryGamma() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryGamma();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryPi0() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryPi0();                           } // Get secondary particles with the mass constraint
const std::vector< std::vector<KFParticle> >* StKFParticleInterface::GetPrimaryCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetPrimaryCandidates();                } // Get primary particles with the mass constraint
const std::vector< std::vector<KFParticle> >* StKFParticleInterface::GetPrimaryTopoCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetPrimaryTopoCandidates();        } // Get primary particles with the topologigal constraint
const std::vector< std::vector<KFParticle> >* StKFParticleInterface::GetPrimaryTopoMassCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetPrimaryTopoMassCandidates();} // Get primary particles with the topologigal and mass constraint

KFParticleFinder*  StKFParticleInterface::GetKFParticleFinder() { return fKFParticleTopoReconstructor->GetKFParticleFinder(); }
void StKFParticleInterface::SetMaxDistanceBetweenParticlesCut(float cut) { GetKFParticleFinder()->SetMaxDistanceBetweenParticlesCut(cut); }
void StKFParticleInterface::SetLCut(float cut)                           { GetKFParticleFinder()->SetLCut(cut); }
void StKFParticleInterface::SetChiPrimaryCut2D(float cut)                { GetKFParticleFinder()->SetChiPrimaryCut2D(cut); }
void StKFParticleInterface::SetChi2Cut2D(float cut)                      { GetKFParticleFinder()->SetChi2Cut2D(cut); }
void StKFParticleInterface::SetLdLCut2D(float cut)                       { GetKFParticleFinder()->SetLdLCut2D(cut); }
void StKFParticleInterface::SetLdLCutXiOmega(float cut)                  { GetKFParticleFinder()->SetLdLCutXiOmega(cut); }
void StKFParticleInterface::SetChi2TopoCutXiOmega(float cut)             { GetKFParticleFinder()->SetChi2TopoCutXiOmega(cut); }
void StKFParticleInterface::SetChi2CutXiOmega(float cut)                 { GetKFParticleFinder()->SetChi2CutXiOmega(cut); }
void StKFParticleInterface::SetChi2TopoCutResonances(float cut)          { GetKFParticleFinder()->SetChi2TopoCutResonances(cut); }
void StKFParticleInterface::SetChi2CutResonances(float cut)              { GetKFParticleFinder()->SetChi2CutResonances(cut); }
void StKFParticleInterface::SetPtCutLMVM(float cut)                      { GetKFParticleFinder()->SetPtCutLMVM(cut); }
void StKFParticleInterface::SetPCutLMVM(float cut)                       { GetKFParticleFinder()->SetPCutLMVM(cut); }
void StKFParticleInterface::SetPtCutJPsi(float cut)                      { GetKFParticleFinder()->SetPtCutJPsi(cut); }
void StKFParticleInterface::SetPtCutCharm(float cut)                     { GetKFParticleFinder()->SetPtCutCharm(cut); }
void StKFParticleInterface::SetChiPrimaryCutCharm(float cut)             { GetKFParticleFinder()->SetChiPrimaryCutCharm(cut); }
void StKFParticleInterface::SetLdLCutCharmManybodyDecays(float cut)      { GetKFParticleFinder()->SetLdLCutCharmManybodyDecays(cut); }
void StKFParticleInterface::SetChi2TopoCutCharmManybodyDecays(float cut) { GetKFParticleFinder()->SetChi2TopoCutCharmManybodyDecays(cut); }
void StKFParticleInterface::SetChi2CutCharmManybodyDecays(float cut)     { GetKFParticleFinder()->SetChi2CutCharmManybodyDecays(cut); }
void StKFParticleInterface::SetLdLCutCharm2D(float cut)                  { GetKFParticleFinder()->SetLdLCutCharm2D(cut); }
void StKFParticleInterface::SetChi2TopoCutCharm2D(float cut)             { GetKFParticleFinder()->SetChi2TopoCutCharm2D(cut); }
void StKFParticleInterface::SetChi2CutCharm2D(float cut)                 { GetKFParticleFinder()->SetChi2CutCharm2D(cut); }
void StKFParticleInterface::SetSecondaryCuts(const float sigmaMass, const float chi2Topo, const float ldl) { GetKFParticleFinder()->SetSecondaryCuts(sigmaMass, chi2Topo, ldl); }
  
double StKFParticleInterface::InversedChi2Prob(double p, int ndf) const
{
  double epsilon = 1.e-14;
  double chi2Left = 0.f;
  double chi2Right = 10000.f;
  
  double probLeft = p - TMath::Prob(chi2Left, ndf);
  
  double chi2Centr = (chi2Left+chi2Right)/2.f;
  double probCentr = p - TMath::Prob( chi2Centr, ndf);
  
  while( TMath::Abs(chi2Right-chi2Centr)/chi2Centr > epsilon )
  {
    if(probCentr * probLeft > 0.f)
    {
      chi2Left = chi2Centr;
      probLeft = probCentr;
    }
    else
    {
      chi2Right = chi2Centr;
    }
    
    chi2Centr = (chi2Left+chi2Right)/2.f;
    probCentr = p - TMath::Prob( chi2Centr, ndf);
  }
  
  return chi2Centr;
}

void StKFParticleInterface::SetPrimaryProbCut(float prob)
{ 
  fKFParticleTopoReconstructor->SetChi2PrimaryCut( InversedChi2Prob(prob, 2) );
}

void StKFParticleInterface::CollectTrackHistograms()
{
  TDirectory *dirs[2] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory("Tracks")) {
    dirs[0]->mkdir("Tracks");
  }
  dirs[1] = dirs[0]->GetDirectory("Tracks"); assert(dirs[1]);
  dirs[1]->cd();
  
  fTrackHistograms2D[0] = (TH2F *)   dirs[1]->Get("hdEdX");
  if (! fTrackHistograms2D[0]) fTrackHistograms2D[0] = new TH2F("hdEdX", "hdEdX", 1000, 0, 10, 200, 0, 200);

  fTrackHistograms2D[1] = (TH2F *)   dirs[1]->Get("hdEdXPos");
  if (! fTrackHistograms2D[1]) fTrackHistograms2D[1] = new TH2F("hdEdXPos", "hdEdXPos", 1000, 0, 10, 200, 0, 200);
  
  fTrackHistograms2D[2] = (TH2F *)   dirs[1]->Get("hdEdXNeg");
  if (! fTrackHistograms2D[2]) fTrackHistograms2D[2] = new TH2F("hdEdXNeg", "hdEdXNeg", 1000, 0, 10, 200, 0, 200);
  
  fTrackHistograms2D[3] = (TH2F *)   dirs[1]->Get("hdEdXwithToF");
  if (! fTrackHistograms2D[3]) fTrackHistograms2D[3] = new TH2F("hdEdXwithToF", "hdEdXwithToF", 1000, 0, 10, 200, 0, 200);
  
  fTrackHistograms2D[4] = (TH2F *)   dirs[1]->Get("hTofPID");
  if (! fTrackHistograms2D[4]) fTrackHistograms2D[4] = new TH2F("hTofPID", "hTofPID", 300, 0, 15, 2100, -1, 20);

  fTrackHistograms[0] = (TH1F *)   dirs[1]->Get("hNHFTHits");
  if (! fTrackHistograms[0]) fTrackHistograms[0] = new TH1F("hNHFTHits", "hNHFTHits",11, -0.5, 10.5);
  
  fTrackHistograms[1] = (TH1F *)   dirs[1]->Get("hPVError");
  if (! fTrackHistograms[1]) fTrackHistograms[1] = new TH1F("hPVError", "hPVError", 10000, 0, 1);

  fTrackHistograms2D[5] = (TH2F *)   dirs[1]->Get("hPVErrorVsNTracks");
  if (! fTrackHistograms2D[5]) fTrackHistograms2D[5] = new TH2F("hPVErrorVsNTracks", "hPVErrorVsNTracks", 1000, 0, 1000, 1000, 0, 0.5);

  fTrackHistograms2D[6] = (TH2F *)   dirs[1]->Get("hPVErrorVsNPVTracks");
  if (! fTrackHistograms2D[6]) fTrackHistograms2D[6] = new TH2F("hPVErrorVsNPVTracks", "hPVErrorVsNPVTracks", 1000, 0, 1000, 1000, 0, 0.5);

  fTrackHistograms[2] = (TH1F *)   dirs[1]->Get("hPrimaryRatio");
  if (! fTrackHistograms[2]) fTrackHistograms[2] = new TH1F("hPrimaryRatio", "hPrimaryRatio", 100, 0, 1);
  
  fTrackHistograms2D[7] = (TH2F *)   dirs[1]->Get("hSecondaryVsPrimaryTracks");
  if (! fTrackHistograms2D[7]) fTrackHistograms2D[7] = new TH2F("hSecondaryVsPrimaryTracks", "hSecondaryVsPrimaryTracks", 200, 0, 1000, 200, 0, 1000);

  fTrackHistograms2D[8] = (TH2F *)   dirs[1]->Get("hdEdXTofPID");
  if (! fTrackHistograms2D[8]) fTrackHistograms2D[8] = new TH2F("hdEdXTofPID", "hdEdXTofPID", 200, 0, 200, 2100, -1, 20);

  fTrackHistograms2D[9] = (TH2F *)   dirs[1]->Get("hETofPID");
  if (! fTrackHistograms2D[9]) fTrackHistograms2D[9] = new TH2F("hETofPID", "ETofPID", 300, 0, 15, 2100, -1, 20);

  fTrackHistograms2D[10] = (TH2F *)   dirs[1]->Get("hdEdXETofPID");
  if (! fTrackHistograms2D[10]) fTrackHistograms2D[10] = new TH2F("hdEdXETofPID", "hdEdXETofPID", 200, 0, 200, 2100, -1, 20);

  dirs[0]->cd();
  
  fCollectTrackHistograms = true;
}

void StKFParticleInterface::CollectPIDHistograms()
{
  TDirectory *dirs[3] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory("Tracks")) {
    dirs[0]->mkdir("Tracks");
  }
  dirs[1] = dirs[0]->GetDirectory("Tracks"); assert(dirs[1]);
  dirs[1]->cd();
  
  int pdgTrackHisto[NTrackHistoFolders] = { 11, -11, 13, -13, 211, -211, 321, -321, 2212, -2212, 
                                            1000010020, -1000010020, 1000010030, -1000010030, 
                                            1000020030, -1000020030, 1000020040, -1000020040,
                                            1000020060, -1000020060, 1000030060, -1000030060,
                                            1000030070, -1000030070, 1000040070, -1000040070 };
  TString trackFolderName[NTrackHistoFolders] = {"e-", "e+", "mu-", "mu+", "pi+", "pi-", "K+", "K-", "p", "p-", 
                                                 "d", "d-", "t", "t-", "He3", "He3-", "He4", "He4-", 
                                                 "He6", "He6-", "Li6", "Li6-", "Li7", "Li7-", "Be7", "Be7-"};
                    
  for(int iTrackHisto=0; iTrackHisto<NTrackHistoFolders; iTrackHisto++)
  {
    if (!dirs[1]->GetDirectory(trackFolderName[iTrackHisto].Data()))
      dirs[1]->mkdir(trackFolderName[iTrackHisto].Data());
    
    dirs[2] = dirs[1]->GetDirectory(trackFolderName[iTrackHisto].Data()); assert(dirs[2]);
    dirs[2]->cd();
    
    fTrackPdgToHistoIndex[ pdgTrackHisto[iTrackHisto] ] = iTrackHisto;
    
    fHistodEdXTracks[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdX");
    if (! fHistodEdXTracks[iTrackHisto]) fHistodEdXTracks[iTrackHisto] = new TH2F("hdEdX", "hdEdX", 1000, 0, 10, 200, 0, 200);

    fHistodEdXwithToFTracks[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdXwithToF");
    if (! fHistodEdXwithToFTracks[iTrackHisto]) fHistodEdXwithToFTracks[iTrackHisto] = new TH2F("hdEdXwithToF", "hdEdXwithToF", 1000, 0, 10, 200, 0, 200);
  
    fHistoTofPIDTracks[iTrackHisto] = (TH2F *)   dirs[2]->Get("hTofPID");
    if (! fHistoTofPIDTracks[iTrackHisto]) fHistoTofPIDTracks[iTrackHisto] = new TH2F("hTofPID", "hTofPID", 300, 0, 15, 1100, -1, 10);
  
    fHistoMomentumTracks[iTrackHisto] = (TH1F *)   dirs[2]->Get("hMomentum");
    if (! fHistoMomentumTracks[iTrackHisto]) fHistoMomentumTracks[iTrackHisto] = new TH1F("hMomentum", "hMomentum", 1000, 0, 10);
    fHistodEdXPull[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdXPull");
    if (! fHistodEdXPull[iTrackHisto]) fHistodEdXPull[iTrackHisto] = new TH2F("hdEdXPull", "hdEdXPull", 500, 0, 10, 300, -30, 30);
    
    fHistodEdXZ[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdXZ");
    if (! fHistodEdXZ[iTrackHisto]) fHistodEdXZ[iTrackHisto] = new TH2F("hdEdXZ", "hdEdXZ", 500, -5, 5, 140, -1, 6);
    dirs[1]->cd();
  }
  
  dirs[0]->cd();
  
  fCollectPIDHistograms = true;
}

void StKFParticleInterface::CollectPVHistograms()
{
  TDirectory* mainDir = TDirectory::CurrentDirectory(); assert(mainDir);
  if (! mainDir->GetDirectory("PrimaryVertices")) {
    mainDir->mkdir("PrimaryVertices");
  }
  TDirectory* pvDir = mainDir->GetDirectory("PrimaryVertices");
  pvDir->cd();
  
  int iHisto = 0;

  // 0
  TString title("X");
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 4000, -200, 200);
  iHisto++;

  // 1
  title = "Y";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 4000, -200, 200);
  iHisto++;

  // 2
  title = "R";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 2000, 0, 200);
  iHisto++;

  // 3
  title = "Z";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 4400, -220, 220);
  iHisto++;

  // 4
  title = "NTracks";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 1000, 0, 1000);
  iHisto++;
  
  // 5
  title = "NTracksMain";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 1000, 0, 1000);
  iHisto++;

  // 6
  title = "NTracksPileup";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 1000, 0, 1000);
  iHisto++;

  // 7
  title = "NPV";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 10, 0, 10);
  iHisto++;

  // 8
  title = "PrimaryFraction";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 100, 0, 1);
  iHisto++;

  // 9
  title = "PileupFraction";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 100, 0, 1);
  iHisto++;

  // 10
  title = "RestFraction";
  fPVHistograms[iHisto] = (TH1F*) pvDir->Get(title);
  if (! fPVHistograms[iHisto]) fPVHistograms[iHisto] = new TH1F(title, title, 100, 0, 1);
  iHisto++;
  
  int iHisto2D = 0;
  
  title = "XY";
  fPVHistograms2D[iHisto2D] = (TH2F *) pvDir->Get(title);
  if (! fPVHistograms2D[iHisto2D]) fPVHistograms2D[iHisto2D] = new TH2F(title, title, 800, -40, 40, 800, -40, 40);
  iHisto2D++;

  title = "ZR";
  fPVHistograms2D[iHisto2D] = (TH2F *) pvDir->Get(title);
  if (! fPVHistograms2D[iHisto2D]) fPVHistograms2D[iHisto2D] = new TH2F(title, title, 4400, -220, 220, 400, -40, 40);
  iHisto2D++;

  title = "ZRTracks";
  fPVHistograms2D[iHisto2D] = (TH2F *) pvDir->Get(title);
  if (! fPVHistograms2D[iHisto2D]) fPVHistograms2D[iHisto2D] = new TH2F(title, title, 440, -220, 220, 400, 0, 200);
  iHisto2D++;

  title = "ZRTracksRest";
  fPVHistograms2D[iHisto2D] = (TH2F *) pvDir->Get(title);
  if (! fPVHistograms2D[iHisto2D]) fPVHistograms2D[iHisto2D] = new TH2F(title, title, 440, -220, 220, 400, 0, 200);
  iHisto2D++;

  mainDir->cd();
  
  fCollectPVHistograms = true;
}

bool StKFParticleInterface::IsGoodPV(const KFVertex& pv)
{
  bool isGoodPV = (pv.X() > -0.3) && (pv.X() < -0.1) &&
                  (pv.Y() > -0.27) && (pv.Y() < -0.1);
  return isGoodPV;
}

bool StKFParticleInterface::GetTrack(const StDcaGeometry& dcaG, KFPTrack& track, int q, int index)
{
  Double_t xyzp[6], CovXyzp[21];
  dcaG.GetXYZ(xyzp,CovXyzp);
  
  bool goodTrack=1;
  for(int iPar=0; iPar<6; iPar++)
    goodTrack = goodTrack && finite(xyzp[iPar]);
  for(int iC=0; iC<21; iC++)
    goodTrack = goodTrack && finite(CovXyzp[iC]);
  goodTrack &= goodTrack && CovXyzp[0]  >=0.f && CovXyzp[0]  < 100.f;
  goodTrack &= goodTrack && CovXyzp[2]  >=0.f && CovXyzp[2]  < 100.f;
  goodTrack &= goodTrack && CovXyzp[5]  >=0.f && CovXyzp[5]  < 100.f;
  goodTrack &= goodTrack && CovXyzp[9]  >=0.f && CovXyzp[9]  < 1.f;
  goodTrack &= goodTrack && CovXyzp[14] >=0.f && CovXyzp[14] < 1.f;
  goodTrack &= goodTrack && CovXyzp[20] >=0.f && CovXyzp[20] < 1.f;
  if(!goodTrack) return false;
  
  track.SetParameters(xyzp);
  track.SetCovarianceMatrix(CovXyzp);
  track.SetNDF(1);
  //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
  track.SetID(index);

  track.SetCharge(q);
  return true;
}

std::vector<int> StKFParticleInterface::GetTofPID(double m2, double p, int q, const int trackId)
{
  static const int order = 4;
  static const double parMean[6][order+1] = { { 0.02283190,-0.01482910, 0.01883130,-0.01824250, 0.00409811  }, //pi+
                                              { 0.24842500,-0.00699781,-0.00991387, 0.01327170,-0.00694824  }, //K+
                                              { 0.863211  , 0.0264171 ,-0.0230833 , 0.00239637, 0.000262309 }, //p
                                              { 0.0224095 ,-0.0123235 , 0.0145216 ,-0.0149944 , 0.00325952  }, //pi-
                                              { 0.250696  ,-0.0151308 , 0.00437457, 0.00516669,-0.00529184  }, //K-
                                              { 0.886912  ,-0.0298543 , 0.0449904 ,-0.0286879 , 0.00541963  }};//p-
  static const double parSigma[6][order+1] = { { 0.0112498,-0.0400571, 0.0733615,-0.0316505, 0.00629469 }, //pi+
                                               { 0.0154830,-0.0396312, 0.0719647,-0.0290683, 0.00637164 }, //K+
                                               { 0.114465 ,-0.287213 , 0.356536 ,-0.169257 , 0.0299844  }, //p
                                               { 0.0111682,-0.0394877, 0.0718342,-0.0302914, 0.00587317 }, //pi-
                                               { 0.0157322,-0.0402606, 0.0716639,-0.0272101, 0.00564467 }, //K-
                                               { 0.0899438,-0.211922 , 0.273122 ,-0.129597 , 0.0231844  }};//p-
  double pMax = 2.;
  double nSigmas[3];
  for(int iHypothesys = 0; iHypothesys<3; iHypothesys++)
  {
    double x = p;
    if(x>=pMax) x = pMax;
    
    int iSet = iHypothesys;
    if(q<0)
      iSet += 3;
    double mean = 0;
    for(int iTerm=0; iTerm<=order; iTerm++)
      mean += parMean[iSet][iTerm]*TMath::Power(x,iTerm);  
    
    double sigma = 0;
    for(int iTerm=0; iTerm<=order; iTerm++)
      sigma += parSigma[iSet][iTerm]*TMath::Power(x,iTerm);  
    
    nSigmas[iHypothesys] = fabs((m2 - mean)/sigma);
    fTrackPidTof[iHypothesys][trackId] = nSigmas[iHypothesys];
  }
  
  double minNSigma = nSigmas[0];
  int minHypothesis = 0;
  for(int iHypothesys=1; iHypothesys<3; iHypothesys++)
  {
    if(minNSigma > nSigmas[iHypothesys]) 
    {
      minNSigma = nSigmas[iHypothesys];
      minHypothesis = iHypothesys;
    }
  }

  int pdgHypothesis[3] = {211, 321, 2212};
  vector<int> tofPID;
  
  if(fStrictTofPID)
  {
    if(minNSigma < 3)
      tofPID.push_back(pdgHypothesis[minHypothesis]*q);
  }
  else
  {    
    for(int iHypothesys=0; iHypothesys<3; iHypothesys++)
      if(nSigmas[iHypothesys] < 3)
        tofPID.push_back(pdgHypothesis[iHypothesys]*q);
  }
  
  return tofPID;
}

std::vector<int> StKFParticleInterface::GetPID(double m2, double p, int q, double dEdX, double dEdXPull[8], bool isBTofm2, bool isETofm2, const int trackId)
{
  const bool isTofm2 = isBTofm2 || isETofm2;

  vector<int> ToFPDG;
  if(isBTofm2)
    ToFPDG = GetTofPID(m2, p, q, trackId);
  
  for(int iPdg=0; iPdg<3; iPdg++)
    fTrackPidTpc[iPdg][trackId] = dEdXPull[iPdg+1];
  
  vector<int> dEdXPDG;
  float nSigmaCut = 3.f; //TODO

#if 1
  bool checkKTof = false;
  if(fCleanKaonsWitTof)
  //   checkKTof = (p > 0.5) && (p < 2.);
    checkKTof = (p > 0.5);
  bool checkKHasTof = 0;
  for(uint32_t iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
    if(abs(ToFPDG[iTofPDG]) == 321)
      checkKHasTof = 1;

  if(dEdXPull[2] < nSigmaCut)                                           dEdXPDG.push_back(211*q);  
  if(dEdXPull[3] < 2.f && ((checkKTof && checkKHasTof) || !checkKTof) ) dEdXPDG.push_back(321*q);
  if(dEdXPull[4] < nSigmaCut)                                           dEdXPDG.push_back(2212*q); 
#else
  bool isKTof = 0;
  bool isPTof = 0;
  bool isPiTof = 0;
  for(uint32_t iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
  {
    if(abs(ToFPDG[iTofPDG]) == 321) isKTof = 1;
    if(abs(ToFPDG[iTofPDG]) == 211) isPiTof = 1;
    if(abs(ToFPDG[iTofPDG]) ==2212) isPTof = 1;
  }
  if(dEdXPull[2] < nSigmaCut && isPiTof) dEdXPDG.push_back(211*q);  
  if(dEdXPull[3] < nSigmaCut && isKTof) dEdXPDG.push_back(321*q);
  if(dEdXPull[4] < nSigmaCut && isPTof) dEdXPDG.push_back(2212*q); 
#endif


  vector<int> totalPDG;
  if(!isBTofm2)
    totalPDG = dEdXPDG;
  else
  {
    for(uint32_t iPDG=0; iPDG<dEdXPDG.size(); iPDG++)
      for(uint32_t iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
        if(dEdXPDG[iPDG] == ToFPDG[iTofPDG])
          totalPDG.push_back(ToFPDG[iTofPDG]);        
  }

  if(dEdXPull[0] < nSigmaCut)  totalPDG.push_back(-11*q);
  if(dEdXPull[1] < nSigmaCut)  totalPDG.push_back(-13*q);
  if(dEdXPull[9] < nSigmaCut  || 
     dEdXPull[10] < nSigmaCut ||
     dEdXPull[11] < nSigmaCut)  
  {
    totalPDG.push_back(2000003112*q);
  }

#if 0 // 2018
  static constexpr double pL[10][4]{
    {7.11737e+00,-1.31428e+00, 1.96720e-01, 6.47905e-02}, // d
    {1.38117e+01,-1.67910e+00,-4.52185e-03, 9.21224e-02}, // t
    {2.27715e+01,-1.36600e+00, 3.01143e-01, 2.38046e-01}, // He3
    {3.45107e+01,-1.22371e+00, 1.89140e-01, 1.07000e-01}, // He4
    {5.63562e+01,-1.29479e+00, 2.27883e-01,-4.10513e-02}, // He6
    {7.30295e+01,-1.08787e+00, 6.87593e-02, 1.14228e-01}, // Li6
    {9.30989e+01,-1.22084e+00, 3.73173e-01,-1.12695e-01}, // Li7
    {1.08163e+02,-1.08057e+00, 2.34159e-01, 1.98949e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  static constexpr double pU[10][4]{
    {1.39824e+01,-1.53919e+00, 1.10899e-01, 9.82910e-02}, // d
    {2.29456e+01,-1.41456e+00, 1.04286e-01, 1.26818e-01}, // t
    {3.33751e+01,-1.22800e+00, 2.98371e-01, 1.82920e-01}, // He3
    {5.09065e+01,-1.36283e+00, 1.90657e-01, 1.98235e-01}, // He4
    {7.52605e+01,-1.42948e+00, 5.87043e-01,-2.13013e-01}, // He6
    {9.35347e+01,-1.25594e+00, 2.91456e-01, 9.52847e-02}, // Li6
    {1.14003e+02,-1.33179e+00, 4.19395e-01,-3.20841e-02}, // Li7
    {1.37012e+02,-1.14016e+00, 3.73116e-01,-1.85678e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  //                                      d    t   He3  He4  He6  Li6  Li7  Be7  He8  Li8
  static constexpr double dedxLower[10]{ 0.0, 0.0, 11., 11., 11., 25., 25., 40., 11., 25. };
  static constexpr double dedxUpper[10]{ 8.0, 8.0, 18., 18., 18., 37., 37., 55., 18., 37. };

#elif 0 // 2021 xProduction, May
  static constexpr double pL[10][4]{
    {7.26450e+00,-1.48942e+00,-2.71718e-01,-8.21588e-02}, // d
    {1.38915e+01,-1.43926e+00,-8.10367e-02,-9.39156e-03}, // t
    {2.23798e+01,-1.09106e+00, 1.35016e-01, 1.72563e-01}, // He3
    {3.12534e+01,-9.52345e-01, 1.63815e-02, 2.03462e-02}, // He4
    {4.59563e+01,-1.05965e+00, 3.70239e-02, 4.92680e-02}, // He6
    {5.33123e+01,-7.79828e-01, 1.13380e-01, 1.39092e-04}, // Li6
    {5.93075e+01,-7.23992e-01, 1.22609e-01,-5.17200e-02}, // Li7
    {7.16577e+01,-8.56517e-01, 2.48878e-01,-3.66107e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  static constexpr double pU[10][4]{
    {1.37151e+01,-1.45881e+00,-3.58669e-02, 3.97500e-02}, // d
    {2.20190e+01,-1.24588e+00, 4.34070e-02, 3.88409e-02}, // t
    {3.09025e+01,-9.75179e-01, 1.03354e-01, 1.29797e-01}, // He3
    {4.10331e+01,-1.01056e+00, 1.98636e-01,-2.77912e-02}, // He4
    {5.52646e+01,-9.50780e-01, 2.14347e-01,-1.03148e-01}, // He6
    {5.81213e+01,-7.88117e-01, 5.20665e-01,-2.26597e-01}, // Li6
    {7.16577e+01,-8.56517e-01, 2.48878e-01,-3.66107e-02}, // Li7
    {1.37012e+02,-1.14016e+00, 3.73116e-01,-1.85678e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  //                                      d    t   He3  He4  He6  Li6  Li7  Be7  He8  Li8
  static constexpr double dedxLower[10]{ 0.0, 0.0, 11., 11., 11., 22., 22., 40., 11., 22. };
  static constexpr double dedxUpper[10]{ 8.0, 8.0, 18., 18., 18., 30., 30., 55., 18., 30. };

#elif 0 // 2021 xProduction, June
  static constexpr double pL[10][4]{
    {7.02144e+00,-1.77017e+00,-2.60061e-01,-1.68402e-02}, // d
    {1.47794e+01,-1.52798e+00,-1.00490e-01, 9.46116e-03}, // t
    {2.46334e+01,-9.18130e-01, 9.93814e-02, 6.08549e-02}, // He3
    {3.37746e+01,-9.67771e-01, 9.44445e-02, 3.52215e-02}, // He4
    {4.94040e+01,-1.03654e+00, 4.80683e-02, 7.45434e-03}, // He6
    {5.79920e+01,-8.01901e-01, 6.66119e-02, 5.34188e-02}, // Li6
    {6.61051e+01,-7.37484e-01,-5.99112e-02, 5.92928e-02}, // Li7
    {8.32337e+01,-7.56362e-01,-5.56798e-02, 8.60781e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  static constexpr double pU[10][4]{
    {1.48953e+01,-1.55755e+00,-2.80406e-01,-5.87432e-02}, // d
    {2.30993e+01,-1.34625e+00, 4.73902e-02, 7.94857e-02}, // t
    {3.33680e+01,-9.69092e-01, 1.15195e-01, 9.37487e-02}, // He3
    {4.42988e+01,-1.07999e+00, 1.50885e-01, 6.51833e-02}, // He4
    {6.71366e+01,-1.20558e+00, 1.63842e-01, 3.51544e-02}, // He6
    {6.66890e+01,-7.82862e-01, 2.14334e-01,-1.15082e-02}, // Li6
    {8.32337e+01,-7.56362e-01,-5.56798e-02, 8.60781e-02}, // Li7
    {1.37012e+02,-1.14016e+00, 3.73116e-01,-1.85678e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  //                                      d    t   He3  He4  He6  Li6  Li7  Be7  He8  Li8
  static constexpr double dedxLower[10]{ 0.0, 0.0, 10., 10., 10., 22., 22., 40., 10., 22. };
  static constexpr double dedxUpper[10]{ 8.0, 8.0, 17., 17., 17., 30., 30., 55., 1., 30. };

#elif 0 // P23id
  static constexpr double pL[10][4]{
    {2.01919e+00, -1.43576e+00, 1.42708e-01, 7.81837e-02},   // d
    {2.75363e+00, -1.47802e+00, -3.68978e-02, -7.81886e-02}, // t
    {3.20869e+00, -1.14289e+00,  2.59129e-01,  2.73274e-02}, // He3
    {3.53401e+00, -1.13341e+00,  2.71542e-01, -3.89132e-02}, // He4
    {4.01727e+00, -1.30366e+00,  4.26304e-01, -1.64219e-01}, // He6
    {4.29368e+00, -1.41286e+00,  4.16766e-01,  3.84379e-02}, // Li6
    {4.57166e+00, -1.33862e+00, -1.90262e-01,  3.41333e-01}, // Li7
    {4.72201e+00, -1.16064e+00,  2.87131e-01,  8.97313e-03}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  static constexpr double pU[10][4]{
    {2.74829e+00, -1.63955e+00, -2.90466e-01, -1.61412e-01}, // d
    {3.17598e+00, -1.24257e+00,  5.97925e-02, -1.03118e-01}, // t
    {3.51253e+00, -1.14517e+00,  3.73812e-01,  5.05007e-02}, // He3
    {3.91966e+00, -1.32891e+00,  4.07485e-01, -1.98109e-02}, // He4
    {4.28499e+00, -1.42002e+00,  4.34652e-01, -7.81419e-02}, // He6
    {4.55036e+00, -1.33616e+00,  1.27882e-01,  2.57759e-01}, // Li6
    {4.79742e+00, -1.29053e+00, -2.93125e-01,  4.32599e-01}, // Li7
    {4.95721e+00, -1.27362e+00,  5.21909e-01, -7.04068e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  //                                      d    t   He3  He4  He6  Li6  Li7  Be7  He8  Li8
  static constexpr double dedxLower[10]{ 0.0, 0.0, 10., 10., 10., 25., 25., 40., 10., 25. };
  static constexpr double dedxUpper[10]{ 8.0, 8.0, 17., 17., 17., 37., 37., 55., 17., 37. };
#else //P24ia

  static constexpr double pL[10][4]{
    {2.0143164938e+00, -1.4868259425e+00, 4.9832096157e-02, 5.0402224369e-02}, // d
    {2.7614255198e+00, -1.5612577498e+00,-1.2128291947e-01, 5.0807142148e-02}, // t
    {3.2507048779e+00, -1.3274318490e+00, 1.1455797223e-01, 2.8319217003e-01}, // He3
    {3.5746170653e+00, -1.0838815425e+00, 2.3482480024e-01,-7.2389968151e-02}, // He4
    {4.0803460926e+00, -1.2931393512e+00, 2.9321341693e-01,-9.8423390129e-02}, // He6
    {4.1007473315e+00, -9.3239946428e-01, 3.6605661735e-01,-1.1129083716e-01}, // Li6
    {4.2184988868e+00, -8.2894179381e-01, 2.3255048995e-01,-1.1203759135e-01}, // Li7
    {4.3625991291e+00, -7.0766460556e-01, 2.7203987630e-01,-8.1081700158e-02}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  static constexpr double pU[10][4]{
    {2.7097536963e+00, -1.5967208060e+00,-1.4348261827e-01, 3.6542475209e-02}, // d
    {3.2004021981e+00, -1.3191149722e+00, 4.6653227329e-03, 2.1260140901e-02}, // t
    {3.5540978373e+00, -1.0937868746e+00, 2.7960853931e-01, 6.1466236409e-02}, // He3
    {3.9196626439e+00, -1.3289103863e+00, 4.0748504441e-01,-1.9810867630e-02}, // He4
    {4.2619406636e+00, -1.1332156925e+00, 1.2847954566e-01, 2.0051492410e-03}, // He6
    {4.5634323873e+00, -1.0855964528e+00, 2.0457422564e-01, 6.9747624209e-02}, // Li6
    {4.6806026804e+00, -9.2998593560e-01,-5.5647040995e-02, 1.3243371161e-01}, // Li7
    {4.9390618150e+00, -1.4423964107e+00, 8.3712434636e-01,-2.0317987761e-01}, // Be7
    {0,0,0,0}, // He8
    {0,0,0,0}, // Li8
  };

  //                                      d    t   He3  He4  He6  Li6  Li7  Be7  He8  Li8
  static constexpr double dedxLower[10]{ 0.0, 0.0, 10., 10., 10., 25., 25., 40., 10., 25. };
  static constexpr double dedxUpper[10]{ 8.0, 8.0, 17., 17., 17., 38., 38., 55., 17., 38. };
#endif

  static constexpr int outPID[10]{
    1000010020, // d
    1000010030, // t
    1000020030, // He3
    1000020040, // He4
    1000020060, // He6
    1000030060, // Li6
    1000030070, // Li7
    1000040070, // Be7
    1000020080, // He8
    1000030080, // Li8
  };

  //                                      0    1   2    3    4    5    6    7    8    9
  //                                      d    t   He3  He4  He6  Li6  Li7  Be7  He8  Li8
  static constexpr double m2Lower[10]  { 3.0, 6.8, 1.0, 3.0, 6.5, 2.8, 4.0, 2.0, 0.0, 5.5 };
  static constexpr double m2Upper[10]  { 4.2, 9.1, 3.0, 4.2, 9.1, 4.2, 6.0, 3.2, 0.0, 6.7 };
  static constexpr double pLimit[10]   { 1.5, 2.5, 3.0, 4.0, 4.5, 4.0, 4.0, 4.0, 0.0, 0.0 };

  double logP = log(p);
  double logP2 = logP * logP;
  double logP3 = logP * logP2;

  bool useTof = true;

  for(int iPID=0; iPID<2; iPID++)
  {
    if( (useTof && isTofm2 && (m2 > m2Lower[iPID] && m2 < m2Upper[iPID])) || !useTof )
    {
      if( p < pLimit[iPID] )
      {
        double lowerBound = exp(pL[iPID][0] + pL[iPID][1] * logP + pL[iPID][2] * logP2 + pL[iPID][3] * logP3);
        double upperBound = exp(pU[iPID][0] + pU[iPID][1] * logP + pU[iPID][2] * logP2 + pU[iPID][3] * logP3);
        
        if(dEdX > lowerBound && dEdX < upperBound)
        {
          totalPDG.push_back(outPID[iPID]*q); 
        }
      }
      else if(dEdXPull[iPID + 5] < nSigmaCut && dEdX < 8.)
      {
        totalPDG.push_back(outPID[iPID]*q);
      }
    }
  }

  if(p > 0.3)
  {
    for(int iPID=2; iPID<8; iPID++)
    {
      if(iPID >= 4 && ! isTofm2) break;

      if( p < pLimit[iPID] )
      {
        double lowerBound = exp(pL[iPID][0] + pL[iPID][1] * logP + pL[iPID][2] * logP2 + pL[iPID][3] * logP3);
        double upperBound = exp(pU[iPID][0] + pU[iPID][1] * logP + pU[iPID][2] * logP2 + pU[iPID][3] * logP3);
        
        if(dEdX > lowerBound && dEdX < upperBound) 
        {
            if( !isTofm2 || (isTofm2 && (m2 > m2Lower[iPID] && m2 < m2Upper[iPID]) ) )
            {
              totalPDG.push_back(outPID[iPID]*q); 
            }
        }
      }
      else if( p >= pLimit[iPID] && dEdX > dedxLower[iPID] && dEdX < dedxUpper[iPID] )
      {
          if( !isTofm2 || (isTofm2 && (m2 > m2Lower[iPID] && m2 < m2Upper[iPID]) ) )
          {
            totalPDG.push_back(outPID[iPID]*q); 
          }
      }
    }
  }

  if(totalPDG.size() == 0)
    totalPDG.push_back(-1);
  
  return totalPDG;
}

inline void Mix2(float& a, float& b, const float sinA, const float cosA)
{
  const float x = a;
  const float y = b;
  a = x * cosA - y * sinA;
  b = x * sinA + y * cosA;
}

void RotateXY2(KFParticle& particle, const float alpha)
{
  const float cosA = std::cos(alpha);
  const float sinA = std::sin(alpha);

  Mix2(particle.Px(), particle.Py(), sinA, cosA);

  Mix2(particle.Covariance(6), particle.Covariance(10), sinA, cosA);
  Mix2(particle.Covariance(7), particle.Covariance(11), sinA, cosA);
  Mix2(particle.Covariance(8), particle.Covariance(12), sinA, cosA);
  Mix2(particle.Covariance(18), particle.Covariance(19), sinA, cosA);
  Mix2(particle.Covariance(24), particle.Covariance(25), sinA, cosA);

  const float c9 = particle.Covariance(9);
  const float c13 = particle.Covariance(13);
  const float c14 = particle.Covariance(14);
  particle.Covariance(9)  = (c9 * cosA - c13 * sinA) * cosA - (c13 * cosA - c14 * sinA) * sinA;
  particle.Covariance(13) = (c13 * cosA + c9 * sinA) * cosA - (c13 * sinA + c14 * cosA) * sinA;
  particle.Covariance(14) = (c13 * cosA + c9 * sinA) * sinA + (c13 * sinA + c14 * cosA) * cosA;
}

void StKFParticleInterface::AddTrackToParticleList(const KFPTrack& track, int nHftHitsInTrack, int index, const std::vector<int>& totalPDG, KFVertex& pv, 
  std::vector<int>& primaryTrackList, std::vector<int>& nHftHits, std::vector<int>& particlesPdg, std::vector<KFParticle>& particles, int& nPartSaved,
  const KFPTrack* trackAtLastHit, std::vector<KFParticle>* particlesAtLastHit)
{
  for(uint32_t iPDG=0; iPDG<totalPDG.size(); iPDG++)
  {
    if( fTriggerMode && (nHftHitsInTrack < 3) ) continue;

    int pdg = totalPDG[iPDG];
    
    KFPTrack trackPDG = track;

    //correct for the charge of ions
    const int index2[9] = { 6,7,8, 10,11,12, 15,16,17 };
    const int index4[6] = { 9, 13,14, 18,19,20 };
//TODO remove coefficient !!!!
#if 1
    {
      trackPDG.SetPx( trackPDG.GetPx()*0.998f );
      trackPDG.SetPy( trackPDG.GetPy()*0.998f );
      trackPDG.SetPz( trackPDG.GetPz()*0.998f );
      for(int iIndex=0; iIndex<9; iIndex++){
        const int iC = index2[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*0.998f );
      }
      for(int iIndex=0; iIndex<6; iIndex++){
        const int iC = index4[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*0.998f*0.998f );
      }
    }
#endif
    if(abs(pdg) == 1000020030 || abs(pdg) == 1000020040 || abs(pdg) == 1000020060) {
      trackPDG.SetCharge( trackPDG.Charge()*2.f );
      trackPDG.SetPx( trackPDG.GetPx()*2.f );
      trackPDG.SetPy( trackPDG.GetPy()*2.f );
      trackPDG.SetPz( trackPDG.GetPz()*2.f );
      for(int iIndex=0; iIndex<9; iIndex++){
        const int iC = index2[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*2.f );
      }
      for(int iIndex=0; iIndex<6; iIndex++){
        const int iC = index4[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*4.f );
      }
    }
    if(abs(pdg) == 1000030060 || abs(pdg) == 1000030070) {
      trackPDG.SetCharge( trackPDG.Charge()*3.f );
      trackPDG.SetPx( trackPDG.GetPx()*3.f );
      trackPDG.SetPy( trackPDG.GetPy()*3.f );
      trackPDG.SetPz( trackPDG.GetPz()*3.f );
      for(int iIndex=0; iIndex<9; iIndex++) {
        const int iC = index2[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*3.f );
      }
      for(int iIndex=0; iIndex<6; iIndex++) {
        const int iC = index4[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*9.f );
      }
    }
    if(abs(pdg) == 1000040070) {
      trackPDG.SetCharge( trackPDG.Charge()*4.f );
      trackPDG.SetPx( trackPDG.GetPx()*4.f );
      trackPDG.SetPy( trackPDG.GetPy()*4.f );
      trackPDG.SetPz( trackPDG.GetPz()*4.f );
      for(int iIndex=0; iIndex<9; iIndex++) {
        const int iC = index2[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*4.f );
      }
      for(int iIndex=0; iIndex<6; iIndex++) {
        const int iC = index4[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*16.f );
      }
    }

    //FIXME uncomment
//     nHftHits[nPartSaved] = nHftHitsInTrack;
    nHftHits[nPartSaved] = 0;
    
    KFParticle particle(trackPDG, pdg);

    float chiPrim = particle.GetDeviationFromVertex(pv);
    if( (chiPrim < fChiPrimaryCut && abs(pdg) < 1000000000) ||
        (chiPrim < fChiPrimaryCutFragments && abs(pdg) >= 1000000000))
    {
      if(fTriggerMode) continue;
      primaryTrackList.push_back(nPartSaved);
    }

    if(fTriggerMode && chiPrim > fChiPrimaryMaxCut) continue;

    particle.SetId(index);
    particles[nPartSaved] = particle;

#ifdef __kfpAtFirstHit__
    if(trackAtLastHit && particlesAtLastHit){
      KFPTrack trackPDGAtLastHit = *trackAtLastHit;

      if(abs(pdg) == 1000020030 || abs(pdg) == 1000020040 || abs(pdg) == 1000020060) {
        trackPDGAtLastHit.SetCharge( trackPDGAtLastHit.Charge()*2.f );
        trackPDGAtLastHit.SetPx( trackPDGAtLastHit.GetPx()*2.f );
        trackPDGAtLastHit.SetPy( trackPDGAtLastHit.GetPy()*2.f );
        trackPDGAtLastHit.SetPz( trackPDGAtLastHit.GetPz()*2.f );
        for(int iIndex=0; iIndex<9; iIndex++){
          const int iC = index2[iIndex];
          trackPDGAtLastHit.SetCovariance( iC, trackPDGAtLastHit.GetCovariance(iC)*2.f );
        }
        for(int iIndex=0; iIndex<6; iIndex++){
          const int iC = index4[iIndex];
          trackPDGAtLastHit.SetCovariance( iC, trackPDGAtLastHit.GetCovariance(iC)*4.f );
        }
      }
      if(abs(pdg) == 1000030060 || abs(pdg) == 1000030070) {
        trackPDGAtLastHit.SetCharge( trackPDGAtLastHit.Charge()*3.f );
        trackPDGAtLastHit.SetPx( trackPDGAtLastHit.GetPx()*3.f );
        trackPDGAtLastHit.SetPy( trackPDGAtLastHit.GetPy()*3.f );
        trackPDGAtLastHit.SetPz( trackPDGAtLastHit.GetPz()*3.f );
        for(int iIndex=0; iIndex<9; iIndex++) {
          const int iC = index2[iIndex];
          trackPDGAtLastHit.SetCovariance( iC, trackPDGAtLastHit.GetCovariance(iC)*3.f );
        }
        for(int iIndex=0; iIndex<6; iIndex++) {
          const int iC = index4[iIndex];
          trackPDGAtLastHit.SetCovariance( iC, trackPDGAtLastHit.GetCovariance(iC)*9.f );
        }
      }
      if(abs(pdg) == 1000040070) {
        trackPDGAtLastHit.SetCharge( trackPDGAtLastHit.Charge()*4.f );
        trackPDGAtLastHit.SetPx( trackPDGAtLastHit.GetPx()*4.f );
        trackPDGAtLastHit.SetPy( trackPDGAtLastHit.GetPy()*4.f );
        trackPDGAtLastHit.SetPz( trackPDGAtLastHit.GetPz()*4.f );
        for(int iIndex=0; iIndex<9; iIndex++) {
          const int iC = index2[iIndex];
          trackPDGAtLastHit.SetCovariance( iC, trackPDGAtLastHit.GetCovariance(iC)*4.f );
        }
        for(int iIndex=0; iIndex<6; iIndex++) {
          const int iC = index4[iIndex];
          trackPDGAtLastHit.SetCovariance( iC, trackPDGAtLastHit.GetCovariance(iC)*16.f );
        }
      }
          
      particlesAtLastHit->at(nPartSaved) = KFParticle(trackPDGAtLastHit, pdg);
      particlesAtLastHit->at(nPartSaved).SetId(index);
    }
#endif

    particlesPdg[nPartSaved] = pdg;

    nPartSaved++;
  }
}

void StKFParticleInterface::FillPIDHistograms(StPicoTrack *gTrack, const std::vector<int>& pdgVector, const bool isTofm2, float m2tof)
{
  float momentum = gTrack->gPtot();
  for(uint32_t iPdg = 0; iPdg<pdgVector.size(); iPdg++)
  {
    int pdg = pdgVector[iPdg];
    const int iTrackHisto = fTrackPdgToHistoIndex[pdg];
    if( ! (iTrackHisto < 0 || iTrackHisto >= NTrackHistoFolders) )
    {
      fHistoMomentumTracks[iTrackHisto] -> Fill(momentum);
      fHistodEdXTracks[iTrackHisto] -> Fill(momentum, gTrack->dEdx());
      if(isTofm2)
      {
        fHistodEdXwithToFTracks[iTrackHisto] -> Fill(momentum, gTrack->dEdx());
        fHistoTofPIDTracks[iTrackHisto] -> Fill(momentum, m2tof);
        if(abs(pdg)==211)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(0.139570, fdEdXMode, 1));
          float betaGamma = TMath::Log10(momentum/0.139570);
          float z = gTrack->dEdxPull(0.139570, fdEdXMode, 1)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
          
          betaGamma = TMath::Log10(momentum/5.485799e-4);
          z = gTrack->nSigmaElectron()*gTrack->dEdxError();
          fHistodEdXZ[0]->Fill(betaGamma, z);
        }
        if(abs(pdg)==321)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(0.493677, fdEdXMode, 1));
          float betaGamma = TMath::Log10(momentum/0.493677);
          float z = gTrack->dEdxPull(0.493677, fdEdXMode, 1)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
        }
        if(abs(pdg)==2212)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(0.938272, fdEdXMode, 1));
          float betaGamma = TMath::Log10(momentum/0.938272);
          float z = gTrack->dEdxPull(0.938272, fdEdXMode, 1)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
        }
        if(abs(pdg)==1000010020)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(1.876124, fdEdXMode, 1));
          float betaGamma = TMath::Log10(momentum/1.876124);
          float z = gTrack->dEdxPull(1.876124, fdEdXMode, 1)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
        }
        if(abs(pdg)==1000010030)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(2.809432, fdEdXMode, 1));
          float betaGamma = TMath::Log10(momentum/2.809432);
          float z = gTrack->dEdxPull(2.809432, fdEdXMode, 1)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
        }
        if(abs(pdg)==1000020030)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(2.809413, fdEdXMode, 2));
          float betaGamma = TMath::Log10(momentum/2.809413);
          float z = gTrack->dEdxPull(2.809413, fdEdXMode, 2)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
        }
        if(abs(pdg)==1000020040)
        {
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(3.728400, fdEdXMode, 2));
          float betaGamma = TMath::Log10(momentum/3.728400);
          float z = gTrack->dEdxPull(3.728400, fdEdXMode, 2)*gTrack->dEdxError();
          fHistodEdXZ[iTrackHisto]->Fill(betaGamma, z);
        }
      }
    }
  }
}

void StKFParticleInterface::FillPIDHistograms(StMuTrack *gTrack, const std::vector<int>& pdgVector, const bool isTofm2, float m2tof)
{
  float momentum = gTrack->p().mag();
  for(uint32_t iPdg = 0; iPdg<pdgVector.size(); iPdg++)
  {
    int pdg = pdgVector[iPdg];
    const int iTrackHisto = fTrackPdgToHistoIndex[pdg];
    if( ! (iTrackHisto < 0 || iTrackHisto >= NTrackHistoFolders) )
    {
      fHistoMomentumTracks[iTrackHisto] -> Fill(momentum);
      fHistodEdXTracks[iTrackHisto] -> Fill(momentum, gTrack->dEdx()*1.e6);
      if(isTofm2)
      {
        fHistodEdXwithToFTracks[iTrackHisto] -> Fill(momentum, gTrack->dEdx()*1.e6);
        fHistoTofPIDTracks[iTrackHisto] -> Fill(momentum, m2tof);
        
        if(abs(pdg)==211)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(0.139570, fdEdXMode, 1));
        if(abs(pdg)==321)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(0.493677, fdEdXMode, 1));
        if(abs(pdg)==2212)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(0.938272, fdEdXMode, 1));
        if(abs(pdg)==1000010020)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(1.876124, fdEdXMode, 1));
        if(abs(pdg)==1000010030)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(2.809432, fdEdXMode, 1));
        if(abs(pdg)==1000020030)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(2.809413, fdEdXMode, 2));
        if(abs(pdg)==1000020040)
          fHistodEdXPull[iTrackHisto] -> Fill(momentum, gTrack->dEdxPull(3.728400, fdEdXMode, 2));
      }
    }
  }
}

bool StKFParticleInterface::OpenCharmTrigger() 
{
  bool triggerDMesons = false;
  if(fKFParticleTopoReconstructor->NPrimaryVertices() == 0) return false;
    
  for(uint32_t iParticle=0; iParticle<GetParticles().size(); iParticle++)
  {
    KFParticle particle = GetParticles()[iParticle];
    
    if( abs(particle.GetPDG()) == 421 ||
        abs(particle.GetPDG()) == 429 || 
        abs(particle.GetPDG()) == 420 || 
        abs(particle.GetPDG()) == 411 || 
        abs(particle.GetPDG()) == 431 || 
        abs(particle.GetPDG()) == 4122 ||
        abs(particle.GetPDG()) == 426 )
    {
      KFParticleSIMD tempSIMDPart(particle);
      float32_v l,dl;
      KFParticleSIMD pv(fKFParticleTopoReconstructor->GetPrimVertex());
      tempSIMDPart.GetDistanceToVertexLine(pv, l, dl);
      
      if(abs(particle.GetPDG()) == 411)
        triggerDMesons = (l[0] < 0.4);
      else    
        triggerDMesons = (l[0] < 0.2);
    }
  }
  
  return triggerDMesons;
}

void StKFParticleInterface::OpenCharmTriggerCompression(int nTracksTriggered, int nTracksInEvent, bool triggerDMesons) 
{
  static int nTriggeredEvents = 0;
  static int nTracksInEventTriggered = 0;
  static int nTracksInEventTotal = 0;
  static int nEvents = 0;
  nEvents++;
  nTracksInEventTotal += nTracksInEvent;
  if(triggerDMesons)
  {
    nTriggeredEvents++;
    nTracksInEventTriggered += nTracksTriggered;
    std::cout << "N Events " << nEvents << "    N triggered events " << nTriggeredEvents << "    ratio " << (double(nEvents)/double(nTriggeredEvents)) << std::endl;
    std::cout << "N Tracks " << nTracksInEventTotal << "    N triggered events " << nTracksInEventTriggered << "    ratio " << (double(nTracksInEventTotal)/double(nTracksInEventTriggered)) << std::endl;
  }
}

void StKFParticleInterface::ResizeTrackPidVectors(const int nTracks)
{
  for(int iHypothesis=0; iHypothesis<3; iHypothesis++)
  {
    fTrackPidTof[iHypothesis].clear();
    fTrackPidTof[iHypothesis].resize(nTracks, -1);
    
    fTrackPidTpc[iHypothesis].clear();
    fTrackPidTpc[iHypothesis].resize(nTracks, -1);
  }
}

bool StKFParticleInterface::ProcessEvent(StPicoDst* picoDst, std::vector<int>& triggeredTracks)
{
  triggeredTracks.resize(0);
  
  //read PV from pico Event
  KFVertex primaryVertex;
  vector<int> primaryTrackList;
    
  StPicoEvent* picoEvent = picoDst->event();
  if(!picoEvent) return 0;
  
  const TVector3 picoPV = picoEvent->primaryVertex();
  const TVector3 picoPVError = picoEvent->primaryVertexError();
  
  KFPVertex primVtx_tmp;
  primVtx_tmp.SetXYZ(picoPV.x(), picoPV.y(), picoPV.z());
  double dx = picoPVError.x();
  double dy = picoPVError.y();
  double dz = picoPVError.z();
  primVtx_tmp.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
  primaryVertex = KFVertex(primVtx_tmp);
//   if(!IsGoodPV(primaryVertex)) return 0;
  
  
#ifdef FXT
  KFVertex myPV;
  std::vector<bool> isPileup;
  std::vector<KFVertex> pileupPv;
  if(!FindFixedTargetPV(picoDst, myPV, pileupPv, isPileup)) return 0;
  primaryVertex = myPV;
#endif
  
 
  Int_t nGlobalTracks = picoDst->numberOfTracks( );
  
  fParticles.resize(nGlobalTracks*10);
  fNHftHits.resize(nGlobalTracks*10);
  fParticlesPdg.resize(nGlobalTracks*10);
  int nPartSaved = 0;
  int nUsedTracks = 0;
  
  std::vector<int> trakIdToI(nGlobalTracks*2);

  for (Int_t iTrack = 0; iTrack < nGlobalTracks; iTrack++) 
  {
    StPicoTrack *gTrack = picoDst->track(iTrack);
    
//     if ( (gTrack->status() != 0) && !(gTrack->isPrimary()) ) continue; //TODO
    
    if (! gTrack)            continue;
    if (! gTrack->charge())  continue;
    if (  gTrack->nHitsFit() < 15) continue;
//     if (  gTrack->nHitsFit() < 10) continue;
#ifdef FXT
    if (  gTrack->dEdxError() < 0.01 || gTrack->dEdxError() > 0.15 ) continue;
#else
    if (  gTrack->dEdxError() < 0.04 || gTrack->dEdxError() > 0.12 ) continue;
#endif
    const uint32_t index = gTrack->id();

#ifdef FXT
    if(isPileup[index]) continue;
#endif
    
    if(index >= trakIdToI.size()) trakIdToI.resize(index+1);
    trakIdToI[index] = iTrack;
    
    int nHftHitsInTrack = 0;
    if(gTrack->hasPxl1Hit()) nHftHitsInTrack++;
    if(gTrack->hasPxl2Hit()) nHftHitsInTrack++;
    if(gTrack->hasIstHit()) nHftHitsInTrack++;
//       if(gTrack->hasSstHit()) nHftHitsInTrack++;
    
    //FIXME temporary solution!!!
    nHftHitsInTrack = gTrack->nHitsFit();
    
    if(fCollectTrackHistograms) fTrackHistograms[0]->Fill(nHftHitsInTrack);
    
//     if(fUseHFTTracksOnly && nHftHitsInTrack < 3) continue;
    if(fUseHFTTracksOnly && !gTrack->hasIstHit()) continue;
    
    StPicoTrackCovMatrix *cov = picoDst->trackCovMatrix(iTrack);
    const StDcaGeometry dcaG = cov->dcaGeometry();
    Int_t q = 1; if (gTrack->charge() < 0) q = -1;
    KFPTrack track;
    if( !GetTrack(dcaG, track, q, index) ) continue;

    KFParticle ppp(track, 211);
    const float ccc = ppp.GetDeviationFromVertex(primaryVertex);
    const bool isCollectPID = ccc < 18.f;

    if(fCollectTrackHistograms && isCollectPID)
    {
      fTrackHistograms2D[0]->Fill(track.GetP(), gTrack->dEdx());
      if(q>0) fTrackHistograms2D[1]->Fill(track.GetP(), gTrack->dEdx());
      else    fTrackHistograms2D[2]->Fill(track.GetP(), gTrack->dEdx());  
    }
    
    double m2tof = -1.e6;
    bool isBTofm2 = false;
    bool isETofm2 = false;
#ifdef USETOF
    if(gTrack->bTofPidTraitsIndex() >= 0)
    {
      const StPicoBTofPidTraits* btofPid = picoDst->btofPidTraits(gTrack->bTofPidTraitsIndex());
      double betaTof2 = btofPid->btofBeta() * btofPid->btofBeta();
      if(fabs(betaTof2) > 1.e-6)
      {
        m2tof = track.GetP()*track.GetP()*(1./betaTof2 - 1.);
        isBTofm2 = true;
      }
//       else
//       {
//         const TVector3 & tofPoint  = btofPid->btofHitPos();
//         StPicoPhysicalHelix innerHelix = gTrack->helix(picoEvent->bField());
//         double lengthTof = fabs( innerHelix.pathLength( tofPoint ));
//         
//         double timeTof = btofPid->btof();
//         if(timeTof > 0. && lengthTof > 0.)
//         {
//           m2tof = track.GetP()*track.GetP()*(1./((lengthTof/timeTof/29.9792458)*(lengthTof/timeTof/29.9792458))-1.);
//           isBTofm2 = true;
//         }
//       }
      
      if(fCollectTrackHistograms && isCollectPID)
      {
        fTrackHistograms2D[3]->Fill(track.GetP(), gTrack->dEdx());
        fTrackHistograms2D[4]->Fill(track.GetP(), m2tof);
        fTrackHistograms2D[8]->Fill(gTrack->dEdx(), m2tof);
      }
    }

    if(fUseETof && gTrack->eTofPidTraitsIndex() >= 0)
    {
      const StPicoETofPidTraits* etofPid = picoDst->etofPidTraits(gTrack->eTofPidTraitsIndex());
      double betaTof2 = etofPid->beta() * etofPid->beta();
      if(fabs(betaTof2) > 1.e-6)
      {
        m2tof = track.GetP()*track.GetP()*(1./betaTof2 - 1.);
        isETofm2 = true;
      }
      if(fCollectTrackHistograms && isCollectPID && isETofm2)
      {
        fTrackHistograms2D[3]->Fill(track.GetP(), gTrack->dEdx());
        fTrackHistograms2D[9]->Fill(track.GetP(), m2tof);
        fTrackHistograms2D[10]->Fill(gTrack->dEdx(), m2tof);
      }
    }
#endif
    double dEdXPull[12] = { fabs(gTrack->dEdxPull(5.109989461E-04, fdEdXMode, 1)), //0 - e
                            fabs(gTrack->dEdxPull(0.1056583745, fdEdXMode, 1)),    //1 - mu
                            fabs(gTrack->dEdxPull(0.139570, fdEdXMode, 1)),        //2 - pi
                            fabs(gTrack->dEdxPull(0.493677, fdEdXMode, 1)),        //3 - K
                            fabs(gTrack->dEdxPull(0.938272, fdEdXMode, 1)),        //4 - p
                            fabs(gTrack->dEdxPull(1.876124, fdEdXMode, 1)),        //5 - d
                            fabs(gTrack->dEdxPull(2.809432, fdEdXMode, 1)),        //6 - t
                            fabs(gTrack->dEdxPull(2.809413, fdEdXMode, 2)),        //7 - He3
                            fabs(gTrack->dEdxPull(3.728400, fdEdXMode, 2)),        //8 - He4
                            fabs(gTrack->dEdxPull(1.197449, fdEdXMode, 1)),        //9 - Sigma
                            fabs(gTrack->dEdxPull(1.32171, fdEdXMode, 1)),         //10- Xi
                            fabs(gTrack->dEdxPull(1.67245, fdEdXMode, 1))};        //11- Omega
    
    vector<int> totalPDG = GetPID(m2tof, track.GetP(), q, gTrack->dEdx(), dEdXPull, isBTofm2, isETofm2, index);

    int nPartSaved0 = nPartSaved;
    AddTrackToParticleList(track, nHftHitsInTrack, index, totalPDG, primaryVertex, primaryTrackList, fNHftHits, fParticlesPdg, fParticles, nPartSaved); 
    
    if(nPartSaved > nPartSaved0) 
      triggeredTracks.push_back(iTrack);
    
    //fill PID histograms if they are created
    if(fCollectPIDHistograms && isCollectPID)
    {
      vector<int> selectedPdg;
      for(int iPdg = nPartSaved0; iPdg<nPartSaved; iPdg++)
        selectedPdg.push_back(fParticlesPdg[iPdg]);
      FillPIDHistograms(gTrack, selectedPdg, isBTofm2 || isETofm2, m2tof);
    }

    nUsedTracks++;
  }
  
  fParticles.resize(nPartSaved);
  fParticlesPdg.resize(nPartSaved);
  fNHftHits.resize(nPartSaved);

  if(nUsedTracks==0) return 0;
  const int nPrimary = primaryTrackList.size();
  if(fCollectTrackHistograms)
  {
    fTrackHistograms[2]->Fill( double(nPrimary)/double(nUsedTracks) );
    fTrackHistograms2D[7]->Fill( nPrimary, (nUsedTracks - nPrimary) );
  }
  
  if( fCleanLowPVTrackEvents && ( 10*nPrimary < (nUsedTracks - nPrimary) ) ) return 0;  
  if( fCleanLowPVTrackEvents && sqrt(dx*dx + dy*dy) > 0.45 ) return 0;

#ifdef FXT
  if( fCleanLowPVTrackEvents && !(primaryVertex.Z() > 196. && primaryVertex.Z() < 204.) ) return 0;
  if( fCleanLowPVTrackEvents && !(primaryVertex.X() > -2.5 && primaryVertex.X() < 2.  ) ) return 0;
  if( fCleanLowPVTrackEvents && !(primaryVertex.Y() > -4.  && primaryVertex.Y() < 0.  ) ) return 0;
  
  if( fCleanLowPVTrackEvents && ( (nUsedTracks - nPrimary) > 150 ) ) return 0;
#else
  if( fCleanLowPVTrackEvents && primaryVertex.GetR() > 2.5 ) return 0;
#endif
  
  const Double_t field = picoEvent->bField();  
  SetField(field);

  CleanPV();
  InitParticles();

  //read PV
  AddPV(primaryVertex, primaryTrackList);
  if(fCollectTrackHistograms)
  {
    fTrackHistograms[1]->Fill(sqrt(dx*dx + dy*dy));
    fTrackHistograms2D[5]->Fill( nPartSaved, sqrt(dx*dx + dy*dy) );
    fTrackHistograms2D[6]->Fill( primaryTrackList.size(), sqrt(dx*dx + dy*dy) );
  }

// #ifdef FXT
//   vector<int> pileupTrackList;
//   for(uint32_t iPileupPv=0; iPileupPv<pileupPv.size(); iPileupPv++) {
//     AddPV(pileupPv[iPileupPv], pileupTrackList);
//   }
// #endif

  //reconstruct short-lived particles
  ReconstructParticles();

  return 1;
}

bool StKFParticleInterface::ProcessEvent(StMuDst* muDst, vector<KFMCTrack>& mcTracks, vector<int>& mcIndices, bool processSignal)
{  
#ifdef __TFG__VERSION__
  mcTracks.resize(muDst->numberOfMcTracks());
  for (uint32_t iMCTrack=0; iMCTrack<muDst->numberOfMcTracks(); iMCTrack++) 
  {
    StMuMcTrack *mcTrack = muDst->MCtrack(iMCTrack);
    if (! mcTrack) continue;    
    KFMCTrack &mcTrackKF = mcTracks[iMCTrack];
    mcTrack->FillKFMCTrack(mcTrackKF);
    mcTrackKF.SetNMCPixelPoints(mcTrack->No_ist_hit() + mcTrack->No_ssd_hit() + mcTrack->No_pix_hit());
  }
#else
  mcTracks.resize(0);
#endif /* __TFG__VERSION__ */  
  //read PV
  KFVertex primaryVertex;
  vector<int> primaryTrackList;

  float bestRank=-1000000;
  // int bestPV=-1;
  double dx = 0., dy = 0., dz = 0.;
  for(uint32_t iPV=0; iPV<muDst->numberOfPrimaryVertices(); iPV++) 
  {
    StMuPrimaryVertex *Vtx = muDst->primaryVertex(iPV);
    if(!Vtx) continue;
    if (bestRank < Vtx->ranking()) {
      bestRank = Vtx->ranking();
      // bestPV = iPV;
    }
    else continue;
    
    //convert StMuPrimaryVertex to KFVertex
    KFPVertex kfVertex;
    kfVertex.SetXYZ(Vtx->position().x(), Vtx->position().y(), Vtx->position().z());
    dx = Vtx->posError().x();
    dy = Vtx->posError().y();
    dz = Vtx->posError().z();
    kfVertex.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
    UShort_t noTracks = Vtx->noTracks();
    kfVertex.SetNContributors(noTracks);
    kfVertex.SetChi2(Vtx->chiSquared());
    primaryVertex = KFVertex(kfVertex);
  }  
//   if(!IsGoodPV(primaryVertex)) return 0;

  Int_t nGlobalTracks = muDst->numberOfGlobalTracks();
  
  fParticles.resize(nGlobalTracks*10);
#ifdef __kfpAtFirstHit__
  fParticlesAtLastHit.resize(nGlobalTracks*10);
#endif
  fNHftHits.resize(nGlobalTracks*10);
  fParticlesPdg.resize(nGlobalTracks*10);
  int nPartSaved = 0;
  int nUsedTracks = 0;
  
  std::vector<int> trakIdToI(nGlobalTracks*2);
  
  for (Int_t iTrack = 0; iTrack < nGlobalTracks; iTrack++) 
  {
    StMuTrack *gTrack = muDst->globalTracks(iTrack);
    if (! gTrack)            continue;
    if (! gTrack->charge())  continue;
    if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) continue; // bad fit or short track pointing to EEMC
    if (  gTrack->flag() > 1000) continue;  // pile up track in TPC
    if (  gTrack->nHitsFit() < 15) continue;
    if (  gTrack->probPidTraits().dEdxErrorFit() < 0.04 || gTrack->probPidTraits().dEdxErrorFit() > 0.12 ) continue;
    
    int nHftHitsInTrack = gTrack->nHitsFit(kIstId) + gTrack->nHitsFit(kSsdId) + gTrack->nHitsFit(kPxlId);
    if(fCollectTrackHistograms) fTrackHistograms[0]->Fill(nHftHitsInTrack);
    if(fUseHFTTracksOnly && nHftHitsInTrack < 3) continue;
    
    const uint32_t index = gTrack->id();
    
    if(index >= trakIdToI.size()) trakIdToI.resize(index+1);
    trakIdToI[index] = iTrack;
    
    int mcIndex = gTrack->idTruth()-1;
    if(mcIndex >= int(mcTracks.size()))
      mcIndex = -1;
    if(mcIndex > -1) {
      if(!processSignal) continue;
    }
    else if(processSignal) continue;
    
    Int_t q = 1; if (gTrack->charge() < 0) q = -1;
#ifdef __kfpAtFirstHit__
    KFPTrack track = gTrack->kfpTrackAtFirstHit();
    KFPTrack trackAtLastHit  = gTrack->kfpTrackAtLastHit();
#else
    Int_t dcaGeometryIndex = gTrack->index2Cov();
    if (dcaGeometryIndex < 0) continue;
    StDcaGeometry *dcaG = StMuDst::instance()->covGlobTracks(dcaGeometryIndex);
    if (! dcaG) continue;
      
    KFPTrack track;
    if( !GetTrack(*dcaG, track, q, index) ) continue;
#endif

    if(fCollectTrackHistograms)
    {
      fTrackHistograms2D[0]->Fill(track.GetP(), gTrack->dEdx()*1.e6);
      if(q>0) fTrackHistograms2D[1]->Fill(track.GetP(), gTrack->dEdx()*1.e6);
      else    fTrackHistograms2D[2]->Fill(track.GetP(), gTrack->dEdx()*1.e6);  
    }
    
    const StMuBTofPidTraits &btofPid = gTrack->btofPidTraits();
    double timeTof = btofPid.timeOfFlight();
    double lengthTof = btofPid.pathLength();
//     if(lengthTof < 0.)
//     {
//       const StThreeVectorF & tofPoint  = btofPid.position();
//       const StThreeVectorF & dcaPoint  = gTrack->dca(bestPV);
//       StPhysicalHelixD innerHelix = gTrack->helix();
//       double dlDCA = fabs( innerHelix.pathLength( StThreeVector<double>(dcaPoint.x(), dcaPoint.y(), dcaPoint.z()) ) );
//       StPhysicalHelixD outerHelix = gTrack->outerHelix();
//       double dlTOF = fabs( outerHelix.pathLength( StThreeVector<double>(tofPoint.x(), tofPoint.y(), tofPoint.z()) ) );
//       
//       double l = gTrack->length();
//       lengthTof = l + dlDCA + dlTOF;
//     }
//     if(lengthTof < 0.)
//     {
//       const StThreeVectorF & tofPoint  = btofPid.position();
//       StPhysicalHelixD innerHelix = dcaG->helix();
//       StMuPrimaryVertex *pv = muDst->primaryVertex(bestPV);
//       lengthTof = fabs( innerHelix.pathLength( StThreeVector<double>(tofPoint.x(), tofPoint.y(), tofPoint.z()) ) ) + 
//                   fabs( innerHelix.pathLength( StThreeVector<double>(pv->position().x(), 
//                                                                      pv->position().y(), 
//                                                                      pv->position().z()) ) );
//     }
    double m2tof = -1.e6;
    bool isTofm2 = false;
#ifdef USETOF
    if(timeTof > 0. && lengthTof > 0.)
    {
      m2tof = track.GetP()*track.GetP()*(1./((lengthTof/timeTof/29.9792458)*(lengthTof/timeTof/29.9792458))-1.);
      isTofm2 = true;
      
      if(fCollectTrackHistograms)
      {
        fTrackHistograms2D[3]->Fill(track.GetP(), gTrack->dEdx()*1.e6);
        fTrackHistograms2D[4]->Fill(track.GetP(), m2tof);
      }
    }
#endif
    double dEdXPull[12] = { fabs(gTrack->nSigmaElectron()),                     //0 - e
                            fabs(gTrack->dEdxPull(0.1056583745, fdEdXMode, 1)), //1 - mu
                            fabs(gTrack->nSigmaPion()),                         //2 - pi
                            fabs(gTrack->nSigmaKaon()),                         //3 - K
                            fabs(gTrack->nSigmaProton()),                       //4 - p
                            fabs(gTrack->dEdxPull(1.876124, fdEdXMode, 1)),     //5 - d
                            fabs(gTrack->dEdxPull(2.809432, fdEdXMode, 1)),     //6 - t
                            fabs(gTrack->dEdxPull(2.809413, fdEdXMode, 2)),     //7 - He3
                            fabs(gTrack->dEdxPull(3.728400, fdEdXMode, 2)),     //8 - He4
                            fabs(gTrack->dEdxPull(1.197449, fdEdXMode, 1)),     //9 - Sigma
                            fabs(gTrack->dEdxPull(1.32171, fdEdXMode, 1)),      //10- Xi
                            fabs(gTrack->dEdxPull(1.67245, fdEdXMode, 1))       //11- Omega
    };    
    
    vector<int> totalPDG = GetPID(m2tof, track.GetP(), q, gTrack->dEdx()*1.e6, dEdXPull, isTofm2, false, index);
    
    int nPartSaved0 = nPartSaved;
    uint32_t nPrimaryTracks = primaryTrackList.size();
#ifdef __kfpAtFirstHit__
    AddTrackToParticleList(track, nHftHitsInTrack, index, totalPDG, primaryVertex, primaryTrackList, fNHftHits, fParticlesPdg, fParticles, nPartSaved, &trackAtLastHit, &fParticlesAtLastHit);
#else
    AddTrackToParticleList(track, nHftHitsInTrack, index, totalPDG, primaryVertex, primaryTrackList, fNHftHits, fParticlesPdg, fParticles, nPartSaved);
#endif

    
    if(nPartSaved > nPartSaved0)
      mcIndices[index] = mcIndex;

    //fill PID histograms if they are created
    //Histograms are filled for secondary tracks only!!!
    if(fCollectPIDHistograms && (nPrimaryTracks == primaryTrackList.size())) 
    {
      vector<int> selectedPdg;
      for(int iPdg = nPartSaved0; iPdg<nPartSaved; iPdg++)
        selectedPdg.push_back(fParticlesPdg[iPdg]);
      FillPIDHistograms(gTrack, selectedPdg, isTofm2, m2tof);
    }
    
    nUsedTracks++;
  }

  fParticles.resize(nPartSaved);
#ifdef __kfpAtFirstHit__
  fParticlesAtLastHit.resize(nPartSaved);
#endif
  fParticlesPdg.resize(nPartSaved);
  fNHftHits.resize(nPartSaved);

  if(nUsedTracks==0) return 0;
  const int nPrimary = primaryTrackList.size();
  if(fCollectTrackHistograms)
  {
    fTrackHistograms[2]->Fill( double(nPrimary)/double(nUsedTracks) );
    fTrackHistograms2D[7]->Fill( nPrimary, (nUsedTracks - nPrimary) );
  }
  
  if( fCleanLowPVTrackEvents && ( 10*nPrimary < (nUsedTracks - nPrimary) ) ) return 0;  
  if( fCleanLowPVTrackEvents && sqrt(dx*dx + dy*dy) > 0.45 ) return 0;
#ifdef FXT
  if( fCleanLowPVTrackEvents && !(primaryVertex.Z() > 196. && primaryVertex.Z() < 204.) ) return 0;
  if( fCleanLowPVTrackEvents && !(primaryVertex.X() > -2.5 && primaryVertex.X() < 2.  ) ) return 0;
  if( fCleanLowPVTrackEvents && !(primaryVertex.Y() > -4.  && primaryVertex.Y() < 0.  ) ) return 0;
  
  if( fCleanLowPVTrackEvents && ( (nUsedTracks - nPrimary) > 150 ) ) return 0;
#else
  if( fCleanLowPVTrackEvents && primaryVertex.GetR() > 2.5 ) return 0;
#endif

  //TODO remove coefficient !!!!
  const Double_t field = muDst->event()->magneticField() * 0.998f;
  SetField(field);

  CleanPV();
#ifdef __kfpAtFirstHit__
  InitParticlesAtFirstAndLastHit();
#else
  InitParticles();
#endif

  //read PV
  AddPV(primaryVertex, primaryTrackList);
  if(fCollectTrackHistograms)
  {
    fTrackHistograms[1]->Fill(sqrt(dx*dx + dy*dy));
    fTrackHistograms2D[5]->Fill( nPartSaved, sqrt(dx*dx + dy*dy) );
    fTrackHistograms2D[6]->Fill( primaryTrackList.size(), sqrt(dx*dx + dy*dy) );
  }  
  //reconstruct short-lived particles
  ReconstructParticles();
  
  return 1;
}


struct pvIndex {
  pvIndex(float d, int j):chi(d), i(j) {}
  float chi;
  int i;
};

static bool sortPVIndices(const pvIndex& a, const pvIndex& b) { return a.chi < b.chi; }

bool StKFParticleInterface::FitPV(KFVertex& pv, bool isFirstSeed, const KFPTrackVector& tracks,
                                  std::vector<int>& pvTrackIndices, std::vector<bool>& isUsed)
{
  std::vector<pvIndex> candidateIndices;
  candidateIndices.reserve(tracks.Size());

  pvTrackIndices.clear();
  pvTrackIndices.reserve(tracks.Size());

  isUsed.clear();
  isUsed.resize(tracks.Size(), false);

  KFParticleSIMD trackSIMD;
  KFParticleSIMD pvSIMD(pv);
  for(int iTrack=0; iTrack < tracks.Size(); iTrack+=SimdLen) {

    trackSIMD.Load(tracks, iTrack);
   
    const float32_v deviation = trackSIMD.GetDeviationFromVertex(pvSIMD);
    
    for(int iV=0; iV<SimdLen; iV++){
      const int iTr = iTrack + iV;
      if(iTr >= tracks.Size()) break;
      
      if((deviation[iV]==deviation[iV]) && (deviation[iV] >= 0.f && deviation[iV] < 200.f))
        candidateIndices.push_back(pvIndex(deviation[iV], iTr));
    }
  }
  
  std::sort(candidateIndices.begin(), candidateIndices.end(), sortPVIndices);
  
  const int nCandidates = candidateIndices.size();
 
  if(nCandidates > 2) {

    vector<bool> vFlags(nCandidates, false);
    
    KFVertex primaryVertex;
    primaryVertex.SetConstructMethod(0);
    for(int iCandidate = 0; iCandidate < nCandidates; iCandidate++) {
      
      KFParticle particle;
      const int iTrack = candidateIndices[iCandidate].i;
      
      particle.Q() = tracks.Q()[iTrack];
      
      particle.X() = tracks.X()[iTrack];
      particle.Y() = tracks.Y()[iTrack];
      particle.Z() = tracks.Z()[iTrack];
      particle.Px() = tracks.Px()[iTrack];
      particle.Py() = tracks.Py()[iTrack];
      particle.Pz() = tracks.Pz()[iTrack];
      
      particle.Covariance( 0) = tracks.Covariance( 0)[iTrack];
      particle.Covariance( 1) = tracks.Covariance( 1)[iTrack];
      particle.Covariance( 2) = tracks.Covariance( 2)[iTrack];
      particle.Covariance( 3) = tracks.Covariance( 3)[iTrack];
      particle.Covariance( 4) = tracks.Covariance( 4)[iTrack];
      particle.Covariance( 5) = tracks.Covariance( 5)[iTrack];
      particle.Covariance( 6) = tracks.Covariance( 6)[iTrack];
      particle.Covariance( 7) = tracks.Covariance( 7)[iTrack];
      particle.Covariance( 8) = tracks.Covariance( 8)[iTrack];
      particle.Covariance( 9) = tracks.Covariance( 9)[iTrack];
      particle.Covariance(10) = tracks.Covariance(10)[iTrack];
      particle.Covariance(11) = tracks.Covariance(11)[iTrack];
      particle.Covariance(12) = tracks.Covariance(12)[iTrack];
      particle.Covariance(13) = tracks.Covariance(13)[iTrack];
      particle.Covariance(14) = tracks.Covariance(14)[iTrack];
      particle.Covariance(15) = tracks.Covariance(15)[iTrack];
      particle.Covariance(16) = tracks.Covariance(16)[iTrack];
      particle.Covariance(17) = tracks.Covariance(17)[iTrack];
      particle.Covariance(18) = tracks.Covariance(18)[iTrack];
      particle.Covariance(19) = tracks.Covariance(19)[iTrack];
      particle.Covariance(20) = tracks.Covariance(20)[iTrack];

      KFVertex tmpPV = primaryVertex;
      tmpPV += particle;
      const float dChi2 = tmpPV.Chi2() - primaryVertex.Chi2();
      if(dChi2 < 6.f) {
        primaryVertex = tmpPV;
        vFlags[iCandidate] = true;
      }
    }

    pv = primaryVertex;

    pvSIMD = KFParticleSIMD(pv);

    for(int iTrack=0; iTrack < tracks.Size(); iTrack+=SimdLen) {
      trackSIMD.Load(tracks, iTrack);
    
      const float32_v deviation = trackSIMD.GetDeviationFromVertex(pvSIMD);
      
      for(int iV=0; iV<SimdLen; iV++){
        const int iTr = iTrack + iV;
        if(iTr >= tracks.Size()) break;
      
        if((deviation[iV]==deviation[iV]) && (deviation[iV] >= 0.f && deviation[iV] < 50.f)) {
          pvTrackIndices.push_back(iTr);
          isUsed[iTr] = true;
        }
      }
    }
    
    if( (isFirstSeed && primaryVertex.NDF() >= 1) || (!isFirstSeed && primaryVertex.NDF() > 7))
    {
      return true;
    }
  }
  
  return false;
}

inline void InvertCholetsky2(float a[3])
{
  const float d0 = 1.f/a[0];
  const float u01 = a[1]*d0;  
  const float d1 = 1.f/(a[2] - u01*a[1]);

  a[2] = d1;
  a[1] = -d1*u01;
  a[0] = d0 - a[1]*u01;
}

struct Point3D {
  Point3D() = default;
  Point3D(const KFParticle& p) {
    m_r[0] = p.X(); m_r[1] = p.Y(); m_r[2] = p.Z();
    m_C[0] = p.GetCovariance(0);
    m_C[1] = p.GetCovariance(1); m_C[2] = p.GetCovariance(2);
    m_C[3] = p.GetCovariance(3); m_C[4] = p.GetCovariance(4); m_C[5] = p.GetCovariance(5);
  }
  
  inline float GetDeviation(const Point3D& p) const {
    float s[6]{ m_C[0] + p.m_C[0], 
                m_C[1] + p.m_C[1], m_C[2] + p.m_C[2],
                m_C[3] + p.m_C[3], m_C[4] + p.m_C[4], m_C[5] + p.m_C[5] };
    KFParticle::InvertCholetsky3(s);
    const float dr[3]{ p.m_r[0] - m_r[0], p.m_r[1] - m_r[1], p.m_r[2] - m_r[2] };
    return  (s[0]*dr[0] + s[1]*dr[1] + s[3]*dr[2])*dr[0]
           +(s[1]*dr[0] + s[2]*dr[1] + s[4]*dr[2])*dr[1]
           +(s[3]*dr[0] + s[4]*dr[1] + s[5]*dr[2])*dr[2];
  }

  inline void Filter(const Point3D& p) {
    const float r[3]{m_r[0], m_r[1], m_r[2]};
    const float c[6]{m_C[0], m_C[1], m_C[2], m_C[3], m_C[4], m_C[5]};
    
    float s[6]{ c[0] + p.m_C[0],
                c[1] + p.m_C[1], c[2] + p.m_C[2],
                c[3] + p.m_C[3], c[4] + p.m_C[4], c[5] + p.m_C[5] };
    KFParticle::InvertCholetsky3(s);
    
    const float k[3][3]{ {c[0]*s[0] + c[1]*s[1] + c[3]*s[3], c[0]*s[1] + c[1]*s[2] + c[3]*s[4], c[0]*s[3] + c[1]*s[4] + c[3]*s[5]},
                         {c[1]*s[0] + c[2]*s[1] + c[4]*s[3], c[1]*s[1] + c[2]*s[2] + c[4]*s[4], c[1]*s[3] + c[2]*s[4] + c[4]*s[5]},
                         {c[3]*s[0] + c[4]*s[1] + c[5]*s[3], c[3]*s[1] + c[4]*s[2] + c[5]*s[4], c[3]*s[3] + c[4]*s[4] + c[5]*s[5]} };
    const float dr[3]{ p.m_r[0] - r[0], p.m_r[1] - r[1], p.m_r[2] - r[2] };

    const float dChi2 = (s[0]*dr[0] + s[1]*dr[1] + s[3]*dr[2])*dr[0]
                       +(s[1]*dr[0] + s[2]*dr[1] + s[4]*dr[2])*dr[1]
                       +(s[3]*dr[0] + s[4]*dr[1] + s[5]*dr[2])*dr[2];

    if((m_chi2 + dChi2) / float(m_ndf + 3) < 10.f) {
      m_r[0] = r[0] + k[0][0]*dr[0] + k[0][1]*dr[1] + k[0][2]*dr[2];
      m_r[1] = r[1] + k[1][0]*dr[0] + k[1][1]*dr[1] + k[1][2]*dr[2];
      m_r[2] = r[2] + k[2][0]*dr[0] + k[2][1]*dr[1] + k[2][2]*dr[2];
        
      m_C[0] = c[0] - k[0][0]*c[0] - k[0][1]*c[1] - k[0][2]*c[3];
      m_C[1] = c[1] - k[1][0]*c[0] - k[1][1]*c[1] - k[1][2]*c[3];
      m_C[2] = c[2] - k[1][0]*c[1] - k[1][1]*c[2] - k[1][2]*c[4];
      m_C[3] = c[3] - k[2][0]*c[0] - k[2][1]*c[1] - k[2][2]*c[3];
      m_C[4] = c[4] - k[2][0]*c[1] - k[2][1]*c[2] - k[2][2]*c[4];
      m_C[5] = c[5] - k[2][0]*c[3] - k[2][1]*c[4] - k[2][2]*c[5];
        
      m_chi2 += dChi2;
      m_ndf += 3;
    }
  }

  static bool compare(const Point3D& a, const Point3D& b) {
    return (a.m_ndf > b.m_ndf) || (a.m_ndf == b.m_ndf && a.m_chi2 < b.m_chi2);
  }
  
  KFParticle convertToKFParticle(const float) const {
    KFParticle p;
    p.X() = m_r[0];
    p.Y() = m_r[1];
    p.Z() = m_r[2];
    p.Covariance(0) = m_C[0];
    p.Covariance(1) = m_C[1];
    p.Covariance(2) = m_C[2];
    p.Covariance(3) = m_C[3];
    p.Covariance(4) = m_C[4];
    p.Covariance(5) = m_C[5];
    return p;
  }
  
  float m_r[3]{0.f};
  float m_C[6]{0.f};
  float m_chi2{0.f};
  int   m_ndf{-2};
};

struct Point2D {
  inline float GetDeviation(const Point2D& p) const {
    float s[3]{ m_C[0] + p.m_C[0], m_C[1] + p.m_C[1], m_C[2] + p.m_C[2] };
    InvertCholetsky2(s);
    const float dr[2]{ p.m_r[0] - m_r[0], p.m_r[1] - m_r[1] };
    return  s[0]*dr[0]*dr[0] + s[1]*dr[0]*dr[1]*2.f + s[2]*dr[1]*dr[1];
  }

  inline void Filter(const Point2D& p) {
    const float r[2]{m_r[0], m_r[1]};
    const float c[3]{m_C[0], m_C[1], m_C[2]};
    
    float s[3]{ c[0] + p.m_C[0], c[1] + p.m_C[1], c[2] + p.m_C[2] };
    InvertCholetsky2(s);
    
    const float k[2][2]{ {c[0] * s[0] + c[1] * s[1], c[0] * s[1] + c[1] * s[2]},
                         {c[1] * s[0] + c[2] * s[1], c[1] * s[1] + c[2] * s[2]} };
    const float dr[2]{ p.m_r[0] - r[0], p.m_r[1] - r[1] };

    const float dChi2 = s[0]*dr[0]*dr[0] + s[1]*dr[0]*dr[1]*2.f + s[2]*dr[1]*dr[1];

    if((m_chi2 + dChi2) / float(m_ndf + 2) < 10.f) {
      m_r[0] = r[0] + k[0][0] * dr[0] + k[0][1] * dr[1];
      m_r[1] = r[1] + k[1][0] * dr[0] + k[1][1] * dr[1];
        
      m_C[0] = c[0] - k[0][0] * c[0] - k[0][1] * c[1];
      m_C[1] = c[1] - k[1][0] * c[0] - k[1][1] * c[1];
      m_C[2] = c[2] - k[1][0] * c[1] - k[1][1] * c[2];
        
      m_chi2 += dChi2;
      m_ndf += 2;
    }
  }

  static bool compare(const Point2D& a, const Point2D& b) {
    return (a.m_ndf > b.m_ndf) || (a.m_ndf == b.m_ndf && a.m_chi2 < b.m_chi2);
  }
  
  float m_r[2]{0.f};
  float m_C[3]{0.f};
  float m_chi2{0.f};
  int   m_ndf{-2};
};

struct PointXY: public Point2D {
  PointXY() = default;
  PointXY(const KFParticle& p) {
    m_r[0] = p.X(); m_r[1] = p.Y();
    m_C[0] = p.GetCovariance(0);
    m_C[1] = p.GetCovariance(1); m_C[2] = p.GetCovariance(2);
  }
  
  KFParticle convertToKFParticle(const float z0) const {
    KFParticle p;
    p.X() = m_r[0];
    p.Y() = m_r[1];
    p.Z() = z0;
    p.Covariance(0) = m_C[0];
    p.Covariance(1) = m_C[1];
    p.Covariance(2) = m_C[2];
    p.Covariance(5) = 9.f;
    return p;
  }
};

struct PointPhiZ: public Point2D {
  PointPhiZ() = default;
  PointPhiZ(const KFParticle& p) {
    const float x = p.X();
    const float y = p.Y();
    const float x2 = x*x;
    const float y2 = y*y;
    const float r2 = x2 + y2;
    const float r4 = r2 * r2;

    const float phi = atan2(y, x);
    
    const float cPhiPhi = (y2 * p.GetCovariance(0) - 2.f*x*y * p.GetCovariance(1) + x2 * p.GetCovariance(2)) / r4;
    const float cPhiZ = (x * p.GetCovariance(4) - y * p.GetCovariance(3)) / r2;
        
    m_r[0] = phi; m_r[1] = p.Z();
    m_C[0] = cPhiPhi;
    m_C[1] = cPhiZ;   m_C[2] = p.GetCovariance(5);
  }
  
  KFParticle convertToKFParticle(const float r) const {
    const float x = r * cos(m_r[0]);
    const float y = r * sin(m_r[0]);
    
    KFParticle p;
    p.X() = x;
    p.Y() = y;
    p.Z() = m_r[1];
    p.Covariance(0) =  y*y * m_C[0];
    p.Covariance(1) = -x*y * m_C[0];
    p.Covariance(2) =  x*x * m_C[0];
    p.Covariance(3) = -y * m_C[1];
    p.Covariance(4) =  x * m_C[1];
    p.Covariance(5) = m_C[2];
    
    return p;
  }
};

struct Position{
  Position(const int i, const float value, const float error2):
    m_i(i), m_value(value), m_error2(error2) {}
  int m_i;
  float m_value;
  float m_error2;
  static bool compare(const Position& a, const Position& b) { return a.m_value > b.m_value; }
};

template<class T>
void getPoints(
  const KFPTrackVector& tracks, const KFVertex& beamPosition, const float R, const float dR,
  std::vector<KFParticle>& points, std::vector<Position>& zHisto);

template<> void getPoints<Point3D>(
  const KFPTrackVector& tracks, const KFVertex& beamPosition, const float R, const float dR,
  std::vector<KFParticle>& points, std::vector<Position>& zHisto)
{
  KFParticleSIMD trackSIMD;
  const float32_v beamXY[2]{beamPosition.X(), beamPosition.Y()};

  for(int iTrack=0; iTrack < tracks.Size(); iTrack+=SimdLen) {
    trackSIMD.Load(tracks, iTrack);
    
    const float32_v ds = trackSIMD.GetDStoPointXY(beamXY);
    float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
    trackSIMD.TransportToDS(ds, dsdr);
    
    for(int iV=0; iV<4; iV++) {
      if(iTrack + iV >= tracks.Size()) continue;
      KFParticle tmp;
      trackSIMD.GetKFParticle(tmp, iV);
      tmp.SetId(iTrack + iV);
     
      const float dx = tmp.X() - beamPosition.X();
      const float dy = tmp.Y() - beamPosition.Y();
      const float xError2 = tmp.GetCovariance(0) + 0.01f;
      const float yError2 = tmp.GetCovariance(2) + 0.01f;
      if( (dx*dx > 10.f*xError2)  || (dy*dy > 10.f*yError2) ) continue;
      if( tmp.GetCovariance(0) > 25.f || tmp.GetCovariance(2) > 25.f || tmp.GetCovariance(5) > 25.f)
        continue;
      
      zHisto.push_back(Position(zHisto.size(), tmp.Z(), tmp.GetCovariance(2,2)));
      points.push_back(tmp);   
    }
  }
}

template<> void getPoints<PointPhiZ>(
  const KFPTrackVector& tracks, const KFVertex& beamPosition, const float R, const float dR,
  std::vector<KFParticle>& points, std::vector<Position>& zHisto)
{
  KFParticleSIMD trackSIMD;
  for(int iTrack=0; iTrack < tracks.Size(); iTrack+=SimdLen) {
    trackSIMD.Load(tracks, iTrack);
    KFParticleSIMD p1 = trackSIMD;
    KFParticleSIMD p2 = trackSIMD;
    float32_v ds[2]{0.f, 0.f};
    p1.GetDStoCylinder(R, ds);
    float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
    p1.TransportToDS(ds[0], dsdr);
    p2.TransportToDS(ds[1], dsdr);
    
    const mask32_v saveFirstPoint = (p1.GetR() > R-dR) && (p1.GetR() < R+dR) && (p1.Z() > -200.f);
    for(int iV=0; iV<4; iV++) {
      if(!saveFirstPoint[iV]) continue;
      if(iTrack + iV >= tracks.Size()) continue;
      KFParticle tmp;
      p1.GetKFParticle(tmp, iV);
      if( tmp.GetCovariance(0) > 25.f || tmp.GetCovariance(2) > 25.f || tmp.GetCovariance(5) > 25.f)
        continue;
      tmp.SetId(iTrack + iV);
      zHisto.push_back(Position(zHisto.size(), tmp.Z(), tmp.GetCovariance(2,2)));
      points.push_back(tmp);
    }
    const mask32_v saveSecondPoint = (p2.GetR() > R-dR) && (p2.GetR() < R+dR) && (p2.Z() > -200.f) &&
                                    (abs(p1.Z() - p2.Z()) > 1.e-4f);
    for(int iV=0; iV<4; iV++) {
      if(!saveSecondPoint[iV]) continue;
      if(iTrack + iV >= tracks.Size()) continue;
      KFParticle tmp;
      p2.GetKFParticle(tmp, iV);
      if( tmp.GetCovariance(0) > 25.f || tmp.GetCovariance(2) > 25.f || tmp.GetCovariance(5) > 25.f)
        continue;
      tmp.SetId(iTrack + iV);
      zHisto.push_back(Position(zHisto.size(), tmp.Z(), tmp.GetCovariance(2,2)));
      points.push_back(tmp);
    }
  }
}

template<> void getPoints<PointXY>(
  const KFPTrackVector& tracks, const KFVertex&, const float Z, const float dZ,
  std::vector<KFParticle>& points, std::vector<Position>& zHisto)
{
  KFParticleSIMD trackSIMD;

  for(int iTrack=0; iTrack < tracks.Size(); iTrack+=SimdLen) {
    trackSIMD.Load(tracks, iTrack);
    
    const float32_v ds = trackSIMD.GetDStoPointZBz(Z);
    float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
    trackSIMD.TransportToDS(ds, dsdr);
    
    for(int iV=0; iV<4; iV++) {
      if(iTrack + iV >= tracks.Size()) continue;
      KFParticle tmp;
      trackSIMD.GetKFParticle(tmp, iV);
      tmp.SetId(iTrack + iV);
     
      const float dz = tmp.Z() - Z;
      if( fabs(dz) > dZ ) continue;
      if( tmp.GetR() > 30.f ) continue;
      if( tmp.GetR() < 3.5f ) continue;
      if( tmp.GetCovariance(0) > 25.f || tmp.GetCovariance(2) > 25.f || tmp.GetCovariance(5) > 25.f)
        continue;
      float rrr, drrr;
      tmp.GetR(rrr, drrr);
      zHisto.push_back(Position(zHisto.size(), rrr, drrr*drrr));
      points.push_back(tmp);   
    }
  }
}


  
template <class T>
inline void cleanCluster(std::vector<int>& cluster, const std::vector<KFParticle>& points)
{
  struct Clean {
    Clean(): m_i(-1), m_id(-1) {}
    Clean(const int i, const int id): m_i(i), m_id(id) {}
    static bool compare(const Clean& a, const Clean& b) { 
      return (a.m_id > b.m_id) || (a.m_id == b.m_id && a.m_i > b.m_i);
    }
    int m_i;
    int m_id;
  };

  std::vector<Clean> clean(cluster.size());
  for(uint32_t i=0; i<cluster.size(); i++) {
    clean[i] = Clean(cluster[i], points[cluster[i]].Id());
  }
  std::sort(clean.begin(), clean.end(), Clean::compare);

  cluster.clear();
  cluster.push_back(clean[0].m_i);
  for(uint32_t i=1; i<clean.size(); i++) {
    if(clean[i].m_id != clean[i-1].m_id) {
      cluster.push_back(clean[i].m_i);
    }
  }
}

template<>
inline void cleanCluster<Point3D>(std::vector<int>& cluster, const std::vector<KFParticle>& points) {}

template<>
inline void cleanCluster<PointXY>(std::vector<int>& cluster, const std::vector<KFParticle>& points) {}

template<class T>
void StKFParticleInterface::FindPileup(const KFPTrackVector& tracks, const KFVertex& beamPosition,
  std::vector<KFVertex>& vertices, std::vector<std::vector<int>>& vertexTracks, const float X, const float dX)
{
  vertices.clear();
  vertexTracks.clear();

  std::vector<KFParticle> points;
  std::vector<Position> zHisto;
  getPoints<T>(tracks, beamPosition, X, dX, points, zHisto);
  std::sort(zHisto.begin(), zHisto.end(), Position::compare);

  // find Z-clusters around each track
  std::vector<std::vector<int>> neighbours(zHisto.size());
  for(uint32_t iZ=0; iZ<zHisto.size(); iZ++) {
    const Position& currentPoint = zHisto[iZ];
    if(currentPoint.m_error2 > 25.f) continue;
    for(uint32_t iZNext = 0; iZNext<zHisto.size(); iZNext++) {
      if(iZNext == iZ) continue;
      const Position& nextPoint = zHisto[iZNext];
      if(nextPoint.m_error2 > 25.f) continue;
      const float dz = currentPoint.m_value - nextPoint.m_value;
      const float zError2 = currentPoint.m_error2 + nextPoint.m_error2;
      if(dz * dz < zError2 * 9 && fabs(dz) < 10.f) {
        neighbours[iZ].push_back(nextPoint.m_i);
      }
    }
    if(neighbours[iZ].size() > 0) {
      neighbours[iZ].push_back(currentPoint.m_i);
      std::sort(neighbours[iZ].begin(), neighbours[iZ].end());
    }
  }
  
  // merge clusters
  for(int i=0; i<2; i++)
  for(uint32_t iZ=0; iZ<neighbours.size(); iZ++) {
    auto& current = neighbours[iZ];
    for(int iZNext=iZ+1; iZNext<neighbours.size(); iZNext++) {
      auto& next = neighbours[iZNext];
      if(next.size() < 2) continue;
      
      std::vector<int> merged;
      std::set_union(current.begin(), current.end(),
                      next.begin(), next.end(),
                      std::back_inserter(merged));
      
      const int intersection = current.size() + next.size() - merged.size();
      
      if( 0.5f * float(min(current.size(), next.size())) < float(intersection) ) {
        current = merged;
        next.clear();
      }
    }
  }
  
  std::vector<bool> isUsed(tracks.Size(), false);
  std::vector<KFVertex> candidateVertices;
  std::vector<std::vector<int>> candidateVertexTracks;

  for(uint32_t iZ=0; iZ<neighbours.size(); iZ++) {
    if(neighbours[iZ].size() == 0) continue;
    std::vector<int> zCluster = neighbours[iZ];
    if(zCluster.size() > 2) {
      std::vector<T> projections(zCluster.size());
      for(uint32_t iPoint=0; iPoint<zCluster.size(); iPoint++) {
        const KFParticle& point = points[zCluster[iPoint]];
        projections[iPoint] = T(point);
      }
    
      std::vector<T> seeds;
      seeds.reserve(zCluster.size());
      for(uint32_t iFirstPoint=0; iFirstPoint<zCluster.size(); iFirstPoint++ ) {
        T phiZSeed = projections[iFirstPoint];
        for(int iIteration=0; iIteration<3; iIteration++) {
          T phiZVertex = projections[iFirstPoint];
          phiZVertex.m_chi2 = 0.f;
          phiZVertex.m_ndf = 0;

          for(uint32_t iPoint=0; iPoint<zCluster.size(); iPoint++) {
            if( iPoint == iFirstPoint ) continue;
            if( phiZSeed.GetDeviation(projections[iPoint]) > 18.6f ) continue;
            
            phiZVertex.Filter(projections[iPoint]);
          }
          phiZSeed = phiZVertex;
        }
        
        if( (phiZSeed.m_ndf >= 4) && (phiZSeed.m_chi2/float(phiZSeed.m_ndf) < 10.f) ) {
          seeds.push_back(phiZSeed);
        }
      }
      
      std::sort(seeds.begin(), seeds.end(), T::compare);
      
      cleanCluster<T>(zCluster, points);

      for(uint32_t iSeed=0; iSeed<seeds.size(); iSeed++) {
        KFParticle seed = seeds[iSeed].convertToKFParticle(X);
        KFParticle vertex;
        int nTracksInVertex = 0;
        std::vector<int> currentVertexTracks;
        
        for(uint32_t iPoint=0; iPoint < zCluster.size(); iPoint++) {
          const KFParticle& point = points[zCluster[iPoint]];
          if(isUsed[point.Id()]) continue;
          if(point.GetDeviationFromVertex(seed) > 18.6f) continue;

          KFParticle vertexTmp = vertex;
          vertexTmp += point;
          if(! (vertexTmp.X() == vertexTmp.X()) ) continue;
          
          vertex = vertexTmp;
          currentVertexTracks.push_back(point.Id());
          nTracksInVertex++;
        }
        
        if(nTracksInVertex > 4 && vertex.Chi2() / vertex.NDF() < 5.f) {
          candidateVertices.push_back(vertex);
          candidateVertexTracks.push_back(currentVertexTracks);
          for(uint32_t iUsed=0; iUsed<currentVertexTracks.size(); iUsed++) {
            isUsed[currentVertexTracks[iUsed]] = true;
          }
        }
      }
    }
  }  

  struct PVCandidate {
    PVCandidate(KFVertex& v, std::vector<int>& t): m_vertex(&v), m_tracks(&t) {}
    
    static bool compare(const PVCandidate& a, const PVCandidate& b) {
      return a.m_tracks->size() > b.m_tracks->size();
    }
    
    KFVertex* m_vertex;
    std::vector<int>* m_tracks;
  };

  std::vector<PVCandidate> candidates;
  for(uint32_t iCandidate=0; iCandidate<candidateVertices.size(); iCandidate++) {
    candidates.push_back(PVCandidate(candidateVertices[iCandidate], candidateVertexTracks[iCandidate]));
  }
  
  std::sort(candidates.begin(), candidates.end(), PVCandidate::compare);
  
  for(uint32_t iCandidate=0; iCandidate<candidates.size(); iCandidate++) {

    KFVertex& vertex = *(candidates[iCandidate].m_vertex);
    std::vector<int> vertexTrackIds = *(candidates[iCandidate].m_tracks);
    
    KFParticleSIMD trackSIMD;
    KFParticleSIMD pvSIMD(vertex);
    for(int iTrack=0; iTrack < tracks.Size(); iTrack+=SimdLen) {
      trackSIMD.Load(tracks, iTrack);
      const float32_v deviation = trackSIMD.GetDeviationFromVertex(pvSIMD);
      for(int iV=0; iV<SimdLen; iV++){
        const int iTr = iTrack + iV;
        if(isUsed[iTr]) continue;
        if(iTr >= tracks.Size()) break;
      
        if((deviation[iV]==deviation[iV]) && (deviation[iV] >= 0.f && deviation[iV] < 18.6f)) {
          vertexTrackIds.push_back(iTr);
          isUsed[iTr] = true;
        }
      }
    }
    
    vertices.push_back(vertex);
    vertexTracks.push_back(vertexTrackIds);
  }
}

void StKFParticleInterface::FillPVHistos(const KFVertex& vertex, const std::vector<int>& tracks, const bool isMainVertex) {
  if(fCollectPVHistograms) {
    fPVHistograms[0]->Fill(vertex.X());
    fPVHistograms[1]->Fill(vertex.Y());
    fPVHistograms[2]->Fill(vertex.GetR());
    fPVHistograms[3]->Fill(vertex.Z());
    fPVHistograms[4]->Fill(tracks.size());
    
    if(isMainVertex) {
      fPVHistograms[5]->Fill(tracks.size());
    } else {
      fPVHistograms[6]->Fill(tracks.size());
    }
    
    fPVHistograms2D[0]->Fill(vertex.X(), vertex.Y());
    fPVHistograms2D[1]->Fill(vertex.Z(), vertex.GetR() * vertex.Y()/fabs(vertex.Y()));    
  }
}

void StKFParticleInterface::CleanTracks(KFPTrackVector& tracks, std::vector<bool>& isUsed, const int nUsed) {
  KFPTrackVector tracksLeft;
  tracksLeft.Resize(tracks.Size() - nUsed);
  
  int iLeft = 0;
  for(int iTrack=0; iTrack<tracks.Size(); iTrack++) {
    if(isUsed[iTrack]) continue;
    
    for(int iP=0; iP<6; iP++)
      tracksLeft.SetParameter(tracks.Parameter(iP)[iTrack], iP, iLeft);
    for(int iC=0; iC<21; iC++)
      tracksLeft.SetCovariance(tracks.Covariance(iC)[iTrack], iC, iLeft);
    tracksLeft.SetId(tracks.Id()[iTrack], iLeft);
    tracksLeft.SetQ(tracks.Q()[iTrack], iLeft);
    iLeft++;
  }
  tracks = tracksLeft;
}

void StKFParticleInterface::CleanPileupTracks(KFPTrackVector& tracks,
  const std::vector<KFVertex>& vertices, std::vector<std::vector<int>>& verticesTracks,
  int& nPV, int& nPileup, std::vector<bool>& isPileup, std::vector<KFVertex>& pileupPv)
{
  std::vector<bool> isUsed(tracks.Size(), false);
  int nUsed = 0;

  for(uint32_t iVertex=0; iVertex<vertices.size(); iVertex++) {
    const KFVertex& vertex = vertices[iVertex];
    const std::vector<int>& vertexTracks = verticesTracks[iVertex];
    
    for(uint32_t iTrack=0; iTrack<vertexTracks.size(); iTrack++) {
      isUsed[vertexTracks[iTrack]] = true;
    }
    nUsed += vertexTracks.size();

    if( (vertex.X() > -2.5f && vertex.X() < 2.f) && (vertex.Y() > -3.f  && vertex.Y() < 0.f) ) {
      pileupPv.push_back(vertex);
    }
    
    for(uint32_t iIndex=0; iIndex<vertexTracks.size(); iIndex++) {
      const int index = tracks.Id()[vertexTracks[iIndex]];
      isPileup[index] = true;
    }
    
    FillPVHistos(vertex, vertexTracks);
    nPileup += vertexTracks.size();
    nPV++;
  }
  CleanTracks(tracks, isUsed, nUsed);
}

bool StKFParticleInterface::FindFixedTargetPV(StPicoDst* picoDst, KFVertex& pv, std::vector<KFVertex>& pileupPv, std::vector<bool>& isPileup)
{
  KFPTrackVector tracks;
  tracks.Resize(picoDst->numberOfTracks());
  
  pileupPv.clear();
  
  int NTracks = 0;
  int maxIndex = 0;
  
  for(uint32_t iTrack=0; iTrack<picoDst->numberOfTracks(); iTrack++) {
    StPicoTrack *gTrack = picoDst->track(iTrack);
    if (! gTrack)            continue;
    if (! gTrack->charge())  continue;
   
    StPicoTrackCovMatrix *cov = picoDst->trackCovMatrix(iTrack);
    const StDcaGeometry dcaG = cov->dcaGeometry();
    const int q = (gTrack->charge() < 0) ? -1 : 1;
    const int index = gTrack->id();
    maxIndex = (index > maxIndex) ? index : maxIndex;
    KFPTrack track;
    if( !GetTrack(dcaG, track, q, index) ) continue;
    
    for(int iP=0; iP<6; iP++)
      tracks.SetParameter(track.GetParameter(iP), iP, NTracks);
    for(int iC=0; iC<21; iC++)
      tracks.SetCovariance(track.GetCovariance(iC), iC, NTracks);
    tracks.SetId(index, NTracks);
    tracks.SetQ(q, NTracks);    
    
    NTracks++;

    if(fCollectPVHistograms) {
      fPVHistograms2D[2]->Fill(track.GetZ(), sqrt(track.GetX()*track.GetX() + track.GetY()*track.GetY()));
    }
  }
  
  tracks.Resize(NTracks);
  isPileup.resize(maxIndex+1, false);

  int nPV = 0;
  int nPrimary = 0;
  int nPileup = 0;
  
  // 1. Find the main vertex

  KFVertex primaryVertex = fBeamSpot;
  std::vector<int> pvTrackIndices;
  std::vector<bool> isUsed;
  const bool isPVFound = FitPV(primaryVertex, true, tracks, pvTrackIndices, isUsed);
  if(isPVFound) {
    pv = primaryVertex;
    FillPVHistos(primaryVertex, pvTrackIndices, true);
    nPrimary += pvTrackIndices.size();
    nPV++;    
  }
  CleanTracks(tracks, isUsed, pvTrackIndices.size());
  
  // 2. Find pileup vertices

  std::vector<KFVertex> pileupVertices;
  std::vector<std::vector<int>> pileupVertexTracks;
  FindPileup<Point3D>(tracks, fBeamSpot, pileupVertices, pileupVertexTracks);
  CleanPileupTracks(tracks, pileupVertices, pileupVertexTracks, nPV, nPileup, isPileup, pileupPv);

  // 3. Find pipe vertices
// KFPTrackVector tracks0 = tracks;
  std::vector<KFVertex> pipeVertices;
  std::vector<std::vector<int>> pipeVertexTracks;
  FindPileup<PointPhiZ>(tracks, fBeamSpot, pipeVertices, pipeVertexTracks);
  CleanPileupTracks(tracks, pipeVertices, pipeVertexTracks, nPV, nPileup, isPileup, pileupPv);
  
  // 4. Find flanec vertices

  std::vector<KFVertex> flanecVertices1;
  std::vector<std::vector<int>> flanecVertexTracks1;
  FindPileup<PointXY>(tracks, fBeamSpot, flanecVertices1, flanecVertexTracks1, 55.5f, 2.f);
  CleanPileupTracks(tracks, flanecVertices1, flanecVertexTracks1, nPV, nPileup, isPileup, pileupPv);

  std::vector<KFVertex> flanecVertices2;
  std::vector<std::vector<int>> flanecVertexTracks2;
  FindPileup<PointXY>(tracks, fBeamSpot, flanecVertices2, flanecVertexTracks2, -55.5f, 2.f);
  CleanPileupTracks(tracks, flanecVertices2, flanecVertexTracks2, nPV, nPileup, isPileup, pileupPv);

  if(fCollectPVHistograms) {
    fPVHistograms[ 7]->Fill(nPV);
    fPVHistograms[ 8]->Fill(float(nPrimary)/float(NTracks));
    fPVHistograms[ 9]->Fill(float(nPileup)/float(NTracks));
    fPVHistograms[10]->Fill(float(NTracks - nPrimary - nPileup)/float(NTracks));
    
    for(int iTrack=0; iTrack<tracks.Size(); iTrack++) 
      fPVHistograms2D[3]->Fill(tracks.Z()[iTrack], 
        sqrt(tracks.X()[iTrack]*tracks.X()[iTrack] + tracks.Y()[iTrack]*tracks.Y()[iTrack]));
  }
  
  return isPVFound;
}

