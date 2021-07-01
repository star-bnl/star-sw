//-*- Mode: C++ -*-
// $Id: AliHLTTPCCASlicesPerformance.h,v 1.4 2010/08/16 23:40:19 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCASLICESPERFORMANCE_H
#define ALIHLTTPCCASLICESPERFORMANCE_H


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

/**
 * @class AliHLTTPCCASlicesPerformance
 */
class AliHLTTPCCASlicesPerformance: public AliHLTTPCCATrackPerformanceBase
{
  public:

    AliHLTTPCCASlicesPerformance():fFirstCall(true){};
    virtual ~AliHLTTPCCASlicesPerformance(){};

    virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                             AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                             AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                             AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
    
      /// Calculate Efficiencies
    virtual void Exec(bool print = false);
    virtual void EfficiencyPerformance();

#ifndef HLTCA_STANDALONE
      /// Histograms
    virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);
    virtual void FillHistos();
#endif

  protected:
    vector<AliHLTTPCCASlicePerformance*> slicePerformances;

    bool fFirstCall; // isn't initialization done yet?
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
