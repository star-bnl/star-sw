//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAMergerPerformance.h,v 1.4 2011/10/26 22:09:08 perev Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 0//def DO_TPCCATRACKER_EFF_PERFORMANCE outdated

#ifndef AliHLTTPCCAMergerPerformance_H
#define AliHLTTPCCAMergerPerformance_H


#include "AliHLTTPCCASlicePerformance.h"


#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include <fstream>
#include <cstdio>
#include <map>

#ifndef HLTCA_STANDALONE
#include "TTree.h"
#include "TObject.h"
#include "TClonesArray.h"
#endif

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

#ifndef HLTCA_STANDALONE
 class MergerPerfDataEvent: public TObject{
 public:
  MergerPerfDataEvent() {};
  virtual ~MergerPerfDataEvent() 
  {
    if(MergerData) delete MergerData;
  }

 friend class AliHLTTPCCAMergerPerformance;
 
 protected:
  TClonesArray *MergerData;
  
  //ClassDef(MergerPerfDataEvent,3)
};
 
class MergerPerfData: public TObject{
 public:
  MergerPerfData() {};
  virtual ~MergerPerfData() {};

  bool IsMerged;
  bool IsDzDs;              // = 1 if dz/ds the same
  bool IsSameTrack;         // = 1 if tracks originated from the same MC track
  bool IsCovMatrixPositive;  // = 1 if the diagonal elements > 0
  bool IsCovMatrixFinite;   // = 1 if the elements are not huge ()
  float chi2;
  float delta2; 
  float Si;
  float dy;
  float dz;
  float dsin;
  
  float nClust1;
  float nClust2;
    
  //ClassDef(MergerPerfData,4)
};
#endif // HLTCA_STANDALONE
 
class AliHLTTPCCAMergerPerformance: public AliHLTTPCCATrackPerformanceBase
{
  public:

    AliHLTTPCCAMergerPerformance();
    virtual ~AliHLTTPCCAMergerPerformance()
    { 
      if(fMPDE) delete fMPDE;
    };

    virtual void SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                             AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                             AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                             AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
    
      /// Calculate Efficiencies
    virtual void Exec(bool print = false);

#ifndef HLTCA_STANDALONE
      /// Histograms
    virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);
    virtual void FillHistos();
    
    void FillTree();
#endif
  
    void AddMergerData(int step, int iSlice1, int iTrack1, int iSlice2, int iTrack2, bool IsDzDs, bool IsCovMatrixPositiv, 
                       bool IsCovMatrixFinite, float &chi2, float &delta2, float &Si, float &dy, float &dz, float &dsin, int nClust1, int nClust2);
    void SetMerged(int step);
    void FillMC();
    
  private:
    int iData;
    int iData2Step;
    vector<AliHLTTPCCASlicePerformance*> slicePerformances;
    bool first_call;
#ifndef HLTCA_STANDALONE
    TTree *MPDTree; // Merger Performance Data tree
    TTree *MPDTree2Step;
    TClonesArray *MergerData;
    MergerPerfDataEvent *fMPDE;
    MergerPerfDataEvent *fMPDE2Step;
#endif
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
