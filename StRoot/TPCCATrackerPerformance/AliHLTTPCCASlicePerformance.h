//-*- Mode: C++ -*-
// $Id: AliHLTTPCCASlicePerformance.h,v 1.3 2013/11/21 13:07:28 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCASLICEPERFORMANCE_H
#define ALIHLTTPCCASLICEPERFORMANCE_H


#include "AliHLTTPCCATrackPerformanceBase.h"

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
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

class TFile;
class AliHLTTPCCATracker;

/**
 * @class AliHLTTPCCASlicePerformance
 */
class AliHLTTPCCASlicePerformance: public AliHLTTPCCATrackPerformanceBase
{
  public:

    AliHLTTPCCASlicePerformance(int iSlice):fISlice(iSlice),firstSliceHit(0),endSliceHit(0){};
    virtual ~AliHLTTPCCASlicePerformance(){};
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
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

#ifndef HLTCA_STANDALONE
      /// Histograms
    virtual void FillHistos();
#endif
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE

    friend class AliHLTTPCCASlicesPerformance;
    friend class AliHLTTPCCAMergerPerformance;
    friend class AliHLTTPCCAGlobalSlicesPerformance;
  protected:
    int fISlice;
    const AliHLTTPCCATracker *sliceTracker;
    int firstSliceHit, endSliceHit;
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
