//-*- Mode: C++ -*-
// $Id: AliHLTTPCCATrackPerformanceBase.h,v 1.3 2013/11/21 13:07:29 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCATrackPERFORMANCEBASE_H
#define ALIHLTTPCCATrackPERFORMANCEBASE_H

#include "AliHLTTPCCounters.h"
#include <algorithm>
using std::sort;

#include "AliHLTTPCPerformanceBase.h"

#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include <fstream>
#include <cstdio>
#include <map>

#include <string>
using std::string;

#include <iostream>
using std::ostream;
using std::istream;

class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class TDirectory;
class TH1;

class TFile;

/**
 * @class AliHLTTPCCATrackPerformanceBase
 */
class AliHLTTPCCATrackPerformanceBase: public AliHLTTPCPerformanceBase
{
 public:

  AliHLTTPCCATrackPerformanceBase();
  virtual ~AliHLTTPCCATrackPerformanceBase(){};

#ifndef HLTCA_STANDALONE
    /// Histograms
  virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);

 protected:

  virtual void FillHistos();


    /// Histos
  enum{ 
    NTracksPulls = 10,
    NHitsPulls = 5+2,
    NHits2DPulls = 3+3+3,

    NAllHisto = 2,
    NAllProfiles = 4,
    NAll2DHisto = 1,
    
    NGhostsHisto = 5,
    NGhostsProfiles = 0,
    NGhosts2DHisto = 2,
        
    NRecoTracksHisto = 8,
    NRecoTracksProfiles = 14,
    NRecoTracks2DHisto = 3
  };
#endif
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
