//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAStiPerformance.h,v 1.1 2010/08/09 21:10:01 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef AliHLTTPCCAStiPerformance_H
#define AliHLTTPCCAStiPerformance_H


#include "AliHLTTPCCAPerformanceBase.h"

#include "AliHLTTPCCAOutTrack.h"

#include "AliHLTTPCCADef.h"
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
class AliHLTTPCCAStiPerformance: public AliHLTTPCCAPerformanceBase
{
  public:

    AliHLTTPCCAStiPerformance(){ };
    virtual ~AliHLTTPCCAStiPerformance(){};

    virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                             AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                             AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                             AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
    
      /// Efficiency
      // Check if MC track is reconstructable. Calculate set of MC track. Etc.
    virtual void CheckMCTracks(); // fill mcData.
      // Find reco-MCTracks correspondence
    virtual void MatchTracks();   // fill recoData.
      // Calculate efficiencies
    virtual void EfficiencyPerformance();

      /// Histograms
    virtual void FillHistos();
    
    int NTracks() {return Tracks.size();}
    void AddTrack(AliHLTTPCCAGBTrack &Tr) {Tracks.push_back(Tr);}
    void AddHit  (int &Hi) {HitIndex.push_back(Hi);}

    
  private:
    int fISlice;
    int firstSliceHit, endSliceHit;
    vector<AliHLTTPCCAGBTrack> Tracks;
    vector<int> HitIndex;
};

#endif
