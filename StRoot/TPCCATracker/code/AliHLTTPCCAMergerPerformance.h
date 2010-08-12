//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAMergerPerformance.h,v 1.1 2010/08/12 19:35:39 mzyzak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef AliHLTTPCCAMergerPerformance_H
#define AliHLTTPCCAMergerPerformance_H


#include "AliHLTTPCCASlicePerformance.h"

#include "AliHLTTPCCADef.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include <fstream>
#include <cstdio>
#include <map>

#include "TTree.h"
#include "TObject.h"
#include "TClonesArray.h"

class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

class TFile;
class TBranch;
class AliHLTTPCCATracker;

/**
 * @class AliHLTTPCCAMergerPerformance
 */
class AliHLTTPCCAMergerPerformance: public AliHLTTPCCAPerformanceBase
{
  public:

    AliHLTTPCCAMergerPerformance():first_call(true){};
    virtual ~AliHLTTPCCAMergerPerformance()
    { 
      if(MPDTree) delete MPDTree;
      if(MergerData) delete MergerData;
    };

    virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                             AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                             AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                             AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
    
      /// Calculate Efficiencies
    virtual void Exec(bool print = false);
    
      /// Histograms
    virtual void CreateHistos(string histoDir = "");
    virtual void FillHistos();
    
    void AddMergerData(int iSlice1, int iTrack1, int iSlice2, int iTrack2, bool IsDzDs, bool IsCovMatrixPositiv, bool IsCovMatrixFinite, float &chi2);
    void WriteHistos();
    
    class MergerPerfData: public TObject{
     public:
      MergerPerfData() {};
      virtual ~MergerPerfData() {};

      bool IsDzDs;              // = 1 if dz/ds the same
      bool IsSameTrack;         // = 1 if tracks originated from the same MC track
      bool IsCovMatrixPositiv;  // = 1 if the diagonal elements > 0
      bool IsCovMatrixFinite;   // = 1 if the elements are not huge ()
      float chi2;
    };
    
  private:
    int iData;
    vector<AliHLTTPCCASlicePerformance*> slicePerformances;
    bool first_call;
    TTree *MPDTree; // Merger Performance Data tree
    TClonesArray *MergerData;
};

#endif
