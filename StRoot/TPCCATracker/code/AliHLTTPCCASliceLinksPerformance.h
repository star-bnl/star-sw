//-*- Mode: C++ -*-
// $Id: AliHLTTPCCASliceLinksPerformance.h,v 1.1 2010/08/16 23:40:19 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCASLICELINKSPERFORMANCE_H
#define ALIHLTTPCCASLICELINKSPERFORMANCE_H


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

struct PerfoTrack
{
  vector<int> hits; // index of hit in global array
};

/**
 * @class AliHLTTPCCASliceLinksPerformance
 */
class AliHLTTPCCASliceLinksPerformance: public AliHLTTPCCASlicePerformance
{
 public:

  AliHLTTPCCASliceLinksPerformance(int iSlice):AliHLTTPCCASlicePerformance(iSlice){};
  virtual ~AliHLTTPCCASliceLinksPerformance(){};

  virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                           AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                           AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                           AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);


    /// Execute standart chain of procedures
  virtual void Exec(bool print = false);
  
    /// Efficiency
    // Find reco-MCTracks correspondence
  virtual void MatchTracks();   // fill recoData.

    /// Histograms // don't need them, so redefine
  virtual void CreateHistos(string histoDir = "", TFile *outFile = 0){};
  virtual void FillHistos(){};

  friend class AliHLTTPCCASlicesLinksPerformance;
 private:
    /// create tracks from linked hits chains
  void CollectTracks();

  vector<PerfoTrack> fRecoTracks;

};



#endif
