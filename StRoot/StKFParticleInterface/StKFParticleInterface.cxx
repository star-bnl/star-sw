#include "StKFParticleInterface.h"

#include "KFParticle/KFParticleTopoReconstructor.h"
#include "KFParticlePerformance/KFMCTrack.h"

#include "TMath.h"
#include "TArrayD.h"

#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoDstMaker/StPicoArrays.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"

#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StProbPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"

ClassImp(StKFParticleInterface)

StKFParticleInterface::StKFParticleInterface(): 
  fKFParticleTopoReconstructor(0), fParticles(0), fParticlesPdg(0), fNHftHits(0), 
  fStrictTofPID(true), fCleanKaonsWitTof(true), fdEdXMode(1), fTriggerMode(false),
  fChiPrimaryCut(18.6)
{
  fKFParticleTopoReconstructor = new KFParticleTopoReconstructor(); // TODO don't recreate with each event

  // set default cuts
  SetPrimaryProbCut(0.0001); // 0.01% to consider primary track as a secondary;
}

StKFParticleInterface::~StKFParticleInterface()
{  
  if(fKFParticleTopoReconstructor) delete fKFParticleTopoReconstructor;
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

void StKFParticleInterface::InitParticles()
{ 
  fKFParticleTopoReconstructor->Init( fParticles, &fParticlesPdg, &fNHftHits );
  Int_t NPV =  fKFParticleTopoReconstructor->NPrimaryVertices();
  fKFParticleTopoReconstructor->GetKFParticleFinder()->Init(NPV);
  fKFParticleTopoReconstructor->FillPVIndices();
}

void StKFParticleInterface::ReconstructParticles()
{ 
  fKFParticleTopoReconstructor->SortTracks();
  fKFParticleTopoReconstructor->ReconstructParticles();
  
//   static int iEvent=0;
//   iEvent++;
//   std::cout << "Event " << iEvent << ": init " << fKFParticleTopoReconstructor->StatTime( 0 ) 
//             << " pv " << fKFParticleTopoReconstructor->StatTime( 1 )
//             << " sort " << fKFParticleTopoReconstructor->StatTime( 2 )
//             << " particles " << fKFParticleTopoReconstructor->StatTime( 3 ) <<  std::endl;
}

void StKFParticleInterface::ReconstructTopology()
{ 
  fKFParticleTopoReconstructor->Init( fParticles, &fParticlesPdg );
  fKFParticleTopoReconstructor->ReconstructPrimVertex(0);
  fKFParticleTopoReconstructor->SortTracks();
  fKFParticleTopoReconstructor->ReconstructParticles();
  
//   static int iEvent=0;
//   iEvent++;
//   std::cout << "Event " << iEvent << ": init " << fKFParticleTopoReconstructor->StatTime( 0 ) 
//             << " pv " << fKFParticleTopoReconstructor->StatTime( 1 )
//             << " sort " << fKFParticleTopoReconstructor->StatTime( 2 )
//             << " particles " << fKFParticleTopoReconstructor->StatTime( 3 ) <<  std::endl;
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
std::vector<KFParticle> const &StKFParticleInterface::GetParticles() const { return fKFParticleTopoReconstructor->GetParticles(); }
const std::vector<KFParticle>* StKFParticleInterface::GetSecondaryCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryCandidates();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryK0() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryK0();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryLambda() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryLambda();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryAntiLambda() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryAntiLambda();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryGamma() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryGamma();                           } // Get secondary particles with the mass constraint
const std::vector<KFParticle>& StKFParticleInterface::GetSecondaryPi0() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetSecondaryPi0();                           } // Get secondary particles with the mass constraint
const std::vector< std::vector<KFParticle> >* StKFParticleInterface::GetPrimaryCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetPrimaryCandidates();                } // Get primary particles with the mass constraint
const std::vector< std::vector<KFParticle> >* StKFParticleInterface::GetPrimaryTopoCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetPrimaryTopoCandidates();        } // Get primary particles with the topologigal constraint
const std::vector< std::vector<KFParticle> >* StKFParticleInterface::GetPrimaryTopoMassCandidates() const {return fKFParticleTopoReconstructor->GetKFParticleFinder()->GetPrimaryTopoMassCandidates();} // Get primary particles with the topologigal and mass constraint

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

bool StKFParticleInterface::IsGoodPV(const KFVertex& pv)
{
  bool isGoodPV = (pv.X() > -0.3) && (pv.X() < -0.1) &&
                  (pv.Y() > -0.27) && (pv.Y() < -0.13);
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

std::vector<int> StKFParticleInterface::GetTofPID(double m2, double p, int q)
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

std::vector<int> StKFParticleInterface::GetPID(double m2, double p, int q, float dEdXPull[7], bool isTofm2)
{
  vector<int> ToFPDG;
//   if(isTofm2)
    ToFPDG = GetTofPID(m2, p, q);
  
  vector<int> dEdXPDG;
  float nSigmaCut = 3.f; //TODO
  
  bool checkKTof = false;
  if(fCleanKaonsWitTof)
    checkKTof = (p > 0.5) && (p < 2.);
  bool checkKHasTof = 0;
  for(unsigned int iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
    if(abs(ToFPDG[iTofPDG]) == 321)
      checkKHasTof = 1;

  if(dEdXPull[0] < nSigmaCut)                                           dEdXPDG.push_back(211*q);  
  if(dEdXPull[1] < 2.f && ((checkKTof && checkKHasTof) || !checkKTof) ) dEdXPDG.push_back(321*q);
  if(dEdXPull[2] < nSigmaCut)                                           dEdXPDG.push_back(2212*q); 
      
  vector<int> totalPDG;
  if(!isTofm2)
    totalPDG = dEdXPDG;
  else
  {
    for(unsigned int iPDG=0; iPDG<dEdXPDG.size(); iPDG++)
      for(unsigned int iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
        if(dEdXPDG[iPDG] == ToFPDG[iTofPDG])
          totalPDG.push_back(ToFPDG[iTofPDG]);        
  }
  
  if( p < 1.5 )
  {    
    if(p > 0.6)
    {
      float sigmaDCut = -3./0.9*(p-0.6) + 3.;
      if(dEdXPull[3] < sigmaDCut) 
        totalPDG.push_back(1000010020*q);
    }
    else
    {
      if(dEdXPull[3] < 3.) totalPDG.push_back(1000010020*q);
    }
  }
  if( p < 1.5 )
  {
    if(p > 0.6)
    {
      float sigmaTCut = -3./0.9*(p-0.6) + 3.;
      if(dEdXPull[4] < sigmaTCut) 
        totalPDG.push_back(1000010030*q);
    }
    else
    {
      if(dEdXPull[4] < 3.) 
        totalPDG.push_back(1000010030*q);
    }        
  }
//     if(p > 1.3)
  {
    if(dEdXPull[5] < nSigmaCut) totalPDG.push_back(1000020030*q);
    if(dEdXPull[6] < nSigmaCut) { totalPDG.push_back(1000020040*q); }
  }
  
  if(totalPDG.size() == 0)
    totalPDG.push_back(-1);
  
  return totalPDG;
}

void StKFParticleInterface::AddTrackToParticleList(const KFPTrack& track, int nHftHitsInTrack, int index, const std::vector<int>& totalPDG, KFVertex& pv, 
  std::vector<int>& primaryTrackList, std::vector<int>& nHftHits, std::vector<int>& particlesPdg, std::vector<KFParticle>& particles, int& nPartSaved)
{
  for(unsigned int iPDG=0; iPDG<totalPDG.size(); iPDG++)
  {
    if( fTriggerMode && (nHftHitsInTrack < 3) ) continue;

    int pdg = totalPDG[iPDG];
    
    KFPTrack trackPDG = track;
    if(abs(pdg) == 1000020030 || abs(pdg) == 1000020040)
    {
      trackPDG.SetCharge( trackPDG.Charge()*2.f );
      trackPDG.SetPx( trackPDG.GetPx()*2.f );
      trackPDG.SetPy( trackPDG.GetPy()*2.f );
      trackPDG.SetPz( trackPDG.GetPz()*2.f );

      const int index2[9] = { 6,7,8, 10,11,12, 15,16,17 };
      for(int iIndex=0; iIndex<9; iIndex++)
      {
        const int iC = index2[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*2.f );
      }
      const int index4[6] = { 9, 13,14, 18,19,20 };
      for(int iIndex=0; iIndex<6; iIndex++)
      {
        const int iC = index4[iIndex];
        trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*4.f );
      }
    }
    
    nHftHits[nPartSaved] = nHftHitsInTrack;
    
    KFParticle particle(trackPDG, pdg);
    float chiPrim = particle.GetDeviationFromVertex(pv);
    if(chiPrim < fChiPrimaryCut)
    {
      if(fTriggerMode) continue;
      primaryTrackList.push_back(nPartSaved);
    }
    particle.SetId(index);
    particles[nPartSaved] = particle;

    particlesPdg[nPartSaved] = pdg;
            
    nPartSaved++;
  }
}

bool StKFParticleInterface::OpenCharmTrigger() 
{
  bool triggerDMesons = false;
  if(fKFParticleTopoReconstructor->NPrimaryVertices() == 0) return false;
    
  for(unsigned int iParticle=0; iParticle<GetParticles().size(); iParticle++)
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
      float_v l,dl;
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

bool StKFParticleInterface::ProcessEvent(StPicoDst* picoDst, std::vector<int>& triggeredTracks)
{
  triggeredTracks.resize(0);
  
  //read PV from pico Event
  KFVertex picoPrimaryVertex;
  vector<int> primaryTrackList;
    
  StPicoEvent* picoEvent = picoDst->event();
  if(!picoEvent) return 0;
  
  const StThreeVectorF picoPV = picoEvent->primaryVertex();
  const StThreeVectorF picoPVError = picoEvent->primaryVertexError();
  
  KFPVertex primVtx_tmp;
  primVtx_tmp.SetXYZ(picoPV.x(), picoPV.y(), picoPV.z());
  double dx = picoPVError.x();
  double dy = picoPVError.y();
  double dz = picoPVError.z();
  primVtx_tmp.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
  picoPrimaryVertex = KFVertex(primVtx_tmp);

  if(!IsGoodPV(picoPrimaryVertex)) return 0;
  
  Int_t nGlobalTracks = picoDst->numberOfTracks( );
  
  //find max global track index
  int maxGBTrackIndex = -1;
  for (Int_t kg = 0; kg < nGlobalTracks; kg++) {
    StPicoTrack *gTrack = picoDst->track(kg);
    if (! gTrack)            continue;
    
    int index = gTrack->id();
    
    if(index > maxGBTrackIndex)
      maxGBTrackIndex = index;
  }
  
  vector<KFParticle> particles(nGlobalTracks*7);
  vector<int> nHftHits(nGlobalTracks*7);
  vector<int> particlesPdg(nGlobalTracks*7);
  int nPartSaved = 0;
  
  for (Int_t kg = 0; kg < nGlobalTracks; kg++) 
  {
    StPicoTrack *gTrack = picoDst->track(kg);
    if (! gTrack)            continue;
    if (! gTrack->charge())  continue;
    if (  gTrack->nHitsFit() < 15) continue;
    if (  gTrack->dEdxError() < 0.04 || gTrack->dEdxError() > 0.12 ) continue;
    const int index = gTrack->id();
    
    const StDcaGeometry dcaG = gTrack->dcaGeometry();
    Int_t q = 1; if (gTrack->charge() < 0) q = -1;
    KFPTrack track;
    if( !GetTrack(dcaG, track, q, index) ) continue;
      
    double m2tof = -1.e6;
    bool isTofm2 = false;
    if(gTrack->bTofPidTraitsIndex() > 0)
    {
      const StPicoBTofPidTraits* btofPid = picoDst->btofPidTraits(gTrack->bTofPidTraitsIndex());
      double betaTof2 = btofPid->btofBeta() * btofPid->btofBeta();
      if(fabs(betaTof2) > 1.e-6)
      {
        m2tof = track.GetP()*track.GetP()*(1./betaTof2 - 1.);
        isTofm2 = true;
      }
    }

    float dEdXPull[7] = { fabs(gTrack->dEdxPull(0.139570, fdEdXMode, 1)),   //0 - pi
                          fabs(gTrack->dEdxPull(0.493677, fdEdXMode, 1)),   //1 - K
                          fabs(gTrack->dEdxPull(0.938272, fdEdXMode, 1)),   //2 - p
                          fabs(gTrack->dEdxPull(1.876124, fdEdXMode, 1)),   //3 - d
                          fabs(gTrack->dEdxPull(2.809432, fdEdXMode, 1)),   //4 - t
                          fabs(gTrack->dEdxPull(2.809413, fdEdXMode, 2)),   //5 - He3
                          fabs(gTrack->dEdxPull(3.728400, fdEdXMode, 2))};  //6 - He4
    
    vector<int> totalPDG = GetPID(m2tof, track.GetP(), q, dEdXPull, isTofm2);
    
    int nHftHitsInTrack = 0;
    if(gTrack->hasPxl1Hit()) nHftHitsInTrack++;
    if(gTrack->hasPxl2Hit()) nHftHitsInTrack++;
    if(gTrack->hasIstHit()) nHftHitsInTrack++;
//       if(gTrack->hasSstHit()) nHftHitsInTrack++;
    int nPartSaved0 = nPartSaved;
    AddTrackToParticleList(track, nHftHitsInTrack, index, totalPDG, picoPrimaryVertex, primaryTrackList, nHftHits, particlesPdg, particles, nPartSaved); 
    
    if(nPartSaved > nPartSaved0) 
      triggeredTracks.push_back(kg);
  }
    
  particles.resize(nPartSaved);
  particlesPdg.resize(nPartSaved);
  nHftHits.resize(nPartSaved);
  
  const Double_t field = picoEvent->bField();
  
  SetField(field);

  SetParticles(particles);
  SetParticlesPdg(particlesPdg);
  SetHftHits(nHftHits);
  CleanPV();
  InitParticles();

  //read PV
  AddPV(picoPrimaryVertex, primaryTrackList);
  
  //reconstruct short-lived particles
  ReconstructParticles();
    
  return 1;
}





