//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGlobalSlicesPerformance.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAGLOBALSLICESPERFORMANCE_H
#define ALIHLTTPCCAGLOBALSLICESPERFORMANCE_H


#include "AliHLTTPCCASlicePerformance.h"

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

#define IsOutTrack1

/**
 * @class AliHLTTPCCAGlobalSlicesPerformance
 */
class AliHLTTPCCAGlobalSlicesPerformance: public AliHLTTPCCAPerformanceBase
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
    virtual void CheckMCTracks(); // fill mcData.
    virtual void EfficiencyPerformance();
    virtual void PrintEfficiencyStatistic();
    
      /// Histograms
    virtual void CreateHistos(string histoDir = "");


  private:
    vector<AliHLTTPCCASlicePerformance*> slicePerformances;

    bool first_call;
};

#endif
