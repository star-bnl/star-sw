//-*- Mode: C++ -*-
// $Id: AliHLTTPCCASlicePerformance.h,v 1.3 2010/08/12 19:35:39 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCASLICEPERFORMANCE_H
#define ALIHLTTPCCASLICEPERFORMANCE_H


#include "AliHLTTPCCAPerformanceBase.h"

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
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

class TFile;
class AliHLTTPCCATracker;

/**
 * @class AliHLTTPCCASlicePerformance
 */
class AliHLTTPCCASlicePerformance: public AliHLTTPCCAPerformanceBase
{
  public:

    AliHLTTPCCASlicePerformance(int iSlice){ fISlice = iSlice; };
    virtual ~AliHLTTPCCASlicePerformance(){};

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
//     virtual void CreateHistos(string histoDir);
    virtual void FillHistos();

    friend class AliHLTTPCCASlicesPerformance;
    friend class AliHLTTPCCAMergerPerformance;
    friend class AliHLTTPCCAGlobalSlicesPerformance;
  private:
    int fISlice;
    const AliHLTTPCCATracker *sliceTracker;
    int firstSliceHit, endSliceHit;
};

#endif
