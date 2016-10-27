//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAStiPerformance.h,v 1.3 2013/11/21 13:07:29 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef AliHLTTPCCAStiPerformance_H
#define AliHLTTPCCAStiPerformance_H


#include "AliHLTTPCCATrackPerformanceBase.h"

#include "AliHLTTPCCAOutTrack.h"

#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include <fstream>
#include <cstdio>
#include <map>

class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class AliHLTTPCCAGBTrack;
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

class TFile;
class AliHLTTPCCATracker;

/**
 * @class AliHLTTPCCAStiPerformance
 */
class AliHLTTPCCAStiPerformance: public AliHLTTPCCATrackPerformanceBase // TODO public GlobalPerfo
{
  public:

    AliHLTTPCCAStiPerformance(){ };
    virtual ~AliHLTTPCCAStiPerformance(){};

    virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const tracker,
                             AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                             AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                             AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
  void SetTracker(const AliHLTTPCCAGBTracker * const tracker) { fTracker = tracker;};
  
      /// Efficiency
      // Check if MC track is reconstructable. Calculate set of MC track. Etc.
    virtual void CheckMCTracks(); // fill mcData.
      // Find reco-MCTracks correspondence
    virtual void MatchTracks();   // fill recoData.
      // Calculate efficiencies
    virtual void EfficiencyPerformance();

 #ifndef HLTCA_STANDALONE
      /// Histograms
    virtual void FillHistos();
  
    virtual void Draw(); // draw diff things after performance
#endif
  
    int NTracks() {return Tracks.size();}
    void SetRecoTracks(vector<AliHLTTPCCAGBTrack> &Trs_){Tracks.clear(); for (unsigned i = 0; i < Trs_.size(); i++) Tracks.push_back(Trs_[i]); }
    void AddHit  (int &Hi) {HitIndex.push_back(Hi);}

    
  private:
    int fISlice;
    int firstSliceHit, endSliceHit;
    vector<AliHLTTPCCAGBTrack> Tracks;
    vector<int> HitIndex;
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
