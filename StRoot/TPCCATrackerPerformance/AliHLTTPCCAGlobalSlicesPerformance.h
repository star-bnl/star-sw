//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGlobalSlicesPerformance.h,v 1.4 2010/08/17 15:47:13 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCAGLOBALSLICESPERFORMANCE_H
#define ALIHLTTPCCAGLOBALSLICESPERFORMANCE_H


#include "AliHLTTPCCASlicePerformance.h"

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

#define IsOutTrack1

/**
 * @class AliHLTTPCCAGlobalSlicesPerformance
 */
class AliHLTTPCCAGlobalSlicesPerformance: public AliHLTTPCCATrackPerformanceBase
{
  public:

    AliHLTTPCCAGlobalSlicesPerformance():first_call(true){};
    virtual ~AliHLTTPCCAGlobalSlicesPerformance(){};

    virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                             AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                             AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                             AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
    
      /// Calculate Efficiencies
    virtual void Exec(bool print = false);
    virtual void CheckMCTracks(); 
    virtual void MatchTracks(); 
  
    virtual void EfficiencyPerformance();

#ifndef HLTCA_STANDALONE
      /// Histograms // don't use them so redefine
    virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);
    virtual void FillHistos(){};
#endif
  
  private:
    vector<AliHLTTPCCASlicePerformance*> slicePerformances;

    bool first_call;
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
