//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCTOPOPERFORMANCE_H
#define ALIHLTTPCTOPOPERFORMANCE_H


#include "KFParticlePerformanceBase.h"

#include "AliHLTArray.h"

#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "KFMCVertex.h"
#include <fstream>
#include <cstdio>
#include <map>

#include "KFPartMatch.h"
#include "KFMCParticle.h"


class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class KFParticleTopoReconstructor;
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

class TFile;
class AliHLTTPCCATracker;

/**
 * @class KFTopoPerformance. Don't use w\o GlobalPerformance
 */
class KFTopoPerformance: public KFParticlePerformanceBase
{
 public:
  
  KFTopoPerformance();
  virtual ~KFTopoPerformance();

  virtual void SetNewEvent(
    const AliHLTTPCCAGBTracker * const Tracker,
    AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
    AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
    AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
  
  void SetNewEvent2( const KFParticleTopoReconstructor * const TopoReconstructor ); // use together with SetNewEvent !!!
    
    /// Efficiency
    // Check if MC track is reconstructable. Calculate set of MC track. Etc.
  virtual void CheckMCTracks(); // fill mcData.
    // Find reco-MCTracks correspondence
  virtual void MatchTracks();   // fill recoData.
    // Calculate efficiencies
  virtual void EfficiencyPerformance(){}; // current don't use eff

  virtual void PrintEfficiencyStatistic(){}; // current don't use eff
  virtual void PrintEfficiency()         {};
  
    /// Histograms
    //     virtual void CreateHistos(string histoDir);
  virtual void FillHistos();

  void AddV0Histos();
  
 private:

  void GetMCParticles();
  void MatchParticles();
  void MatchPV();
  void CalculateEfficiency();
  void CalculatePVEfficiency();
  void FindReconstructableMCParticles();
  void CheckMCParticleIsReconstructable(KFMCParticle &part);
  void FindReconstructableMCVertices();
  
  const KFParticleTopoReconstructor *fTopoReconstructor;

  vector<KFMCVertex> fPrimVertices; // primary vertex positions (currently only one vertex is implemented)
  vector<int> fMCTrackToMCPVMatch; // match between MC tracks and PV 
  vector<double> fPVPurity;
  vector<double> fPVTracksRate[4]; //0 - ghost tracks, 1 - from trigger PV, 2 - from pileup, 3 - from physics bg
  vector<int> fNCorrectPVTracks;

  vector<KFMCParticle> vMCParticles;  // MC particles

  vector<KFPartMatch> MCtoRParticleId; // array for match MC and reco particles
  vector<KFPartMatch> RtoMCParticleId;

  vector<KFPartMatch> MCtoRPVId; // array for match MC and reco PV
  vector<KFPartMatch> RtoMCPVId;
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
