//-*- Mode: C++ -*-
// $Id: AliHLTTPCCASlicesLinksPerformance.h,v 1.3 2013/11/21 13:07:28 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCA_SLICES_LINKS_PERFORMANCE_H
#define ALIHLTTPCCA_SLICES_LINKS_PERFORMANCE_H


#include "AliHLTTPCCASlicesPerformance.h"

#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAGBTracker.h"

  /**
   * @class AliHLTTPCCASlicesLinksPerformance
   */
class AliHLTTPCCASlicesLinksPerformance: public AliHLTTPCCASlicesPerformance
{
 public:

  AliHLTTPCCASlicesLinksPerformance():AliHLTTPCCASlicesPerformance(){};
  virtual ~AliHLTTPCCASlicesLinksPerformance(){};

  virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
  AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
  AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);

    /// Histograms. don't need them
//  virtual void CreateHistos(string histoDir = "", TFile* outFile = 0){};
//  virtual void FillHistos(){};
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
