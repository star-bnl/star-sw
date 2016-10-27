//-*- Mode: C++ -*-
// $Id: AliHLTTPCCASliceLinksPerformance.h,v 1.3 2013/11/21 13:07:28 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCASLICELINKSPERFORMANCE_H
#define ALIHLTTPCCASLICELINKSPERFORMANCE_H


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

    /// Histograms
  // virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);

#ifndef HLTCA_STANDALONE
  virtual void Draw(); // draw links and MC tracks
#endif
  
 protected:
  
#ifndef HLTCA_STANDALONE
  virtual void FillHistos();
#endif

  //   /// Histos
  // enum{ 
  //   NGhostsHisto = 2,
  //   NGhosts2DHisto = 1,
        
  //   NRecoTracksHisto = 2,
  //   NRecoTracks2DHisto = 1
  // };
  
  friend class AliHLTTPCCASlicesLinksPerformance;
 private:
    /// create tracks from linked hits chains
  void CollectTracks();

  vector<PerfoTrack> fRecoTracks;

};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
