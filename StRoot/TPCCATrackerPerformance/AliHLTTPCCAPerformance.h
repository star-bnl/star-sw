//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAPerformance.h,v 1.9 2010/09/01 10:38:27 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCCAPERFORMANCE_H
#define ALIHLTTPCCAPERFORMANCE_H

#include "AliHLTTPCPerformanceBase.h"

// #include "AliHLTTPCCADef.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"

#include <fstream>
#include <cstdio>
#include <map>
#include <string>
#include <vector>
using std::string;

class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class KFParticleTopoReconstructor;
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

/**
 * @class AliHLTTPCCAPerformance
 *
 * Does performance evaluation of the HLT Cellular Automaton-based tracker
 * It checks performance for AliHLTTPCCATracker slice tracker
 * and for AliHLTTPCCAGBTracker global tracker
 *
 */

class AliHLTTPCCAPerformance
{
  public:

    AliHLTTPCCAPerformance();
    virtual ~AliHLTTPCCAPerformance();

      /// initialization before the new event
    bool SetNewEvent(AliHLTTPCCAGBTracker* const Tracker, string mcTracksFile, string mcPointsFile); // set info for new event
    void InitSubPerformances();

      /// Instance
    static AliHLTTPCCAPerformance &Instance();
    
      /// Efficiencies
    void ExecPerformance();

    /// functional is needed by DRAW option. TODO: clean up
  const AliHLTTPCCAMCTrack &MCTrack(int i) const { return fMCTracks[i]; }
  const AliHLTTPCCAHitLabel &HitLabel(int i) const { return fHitLabels[i]; }
  int HitLabelSize() { return fHitLabels.Size(); }
  const AliHLTTPCCAGBTracker *GetTracker(){ return fTracker; };
    

  void SetTracker( AliHLTTPCCAGBTracker* const tracker ){ fTracker = tracker; };
#ifdef KFPARTICLE
  void SetTopoReconstructor( KFParticleTopoReconstructor* const tr ){ fTopoReconstructor = tr; };
#endif
  void SetMCTracks(vector<AliHLTTPCCAMCTrack>& mcTracks);
  void SetMCPoints(vector<AliHLTTPCCALocalMCPoint>& mcPoints);
  void SetHitLabels(vector<AliHLTTPCCAHitLabel>& hitLabels);

  AliHLTResizableArray<AliHLTTPCCAHitLabel>     * GetHitLabels() { return &fHitLabels; }      // array of hit MC labels
  AliHLTResizableArray<AliHLTTPCCAMCTrack>      * GetMCTracks()  { return &fMCTracks;  }      // array of MC tracks
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> * GetMCPoints()  { return &fLocalMCPoints;}   // array of MC points in slices CS

  AliHLTTPCPerformanceBase* GetSubPerformance(string name);
  bool CreateHistos(string name);
  void WriteHistos();
  
  void SetOutputFile(TFile *oF) { fOutputFile = oF; }

  void SaveDataInFiles( string prefix ) const; // Save all MC Data in txt files. @prefix - prefix for file name. Ex: "./data/ev1"
  bool ReadDataFromFiles( string prefix ); // @prefix - prefix for file name. Ex: "./data/ev1"

  void SetRecoData(vector<int> &mcIndexes); //for KFTopoPerformance running without CATracker
  
  protected:

          /// Histograms
    void CreateHistos();
    
          /// Read\write MC information
    void ReadMCEvent( FILE *in );
    void ReadLocalMCPoints( FILE *in );

    void WriteMCEvent( FILE *out ) const;

      /// Sub-pefromances
    struct TSubPerformance{
      AliHLTTPCPerformanceBase* perf;
      string name;
      bool IsGlobalPerf;

      TSubPerformance(){};
      TSubPerformance(AliHLTTPCPerformanceBase* perf_, string name_, bool IsGlobalPerf_ = 1){
        perf = perf_;
        name = name_;
	IsGlobalPerf = IsGlobalPerf_;
#ifndef HLTCA_STANDALONE
	perf->SetHistoCreated(0);
#endif
      };
//       ~TSubPerformance(){if (perf) delete perf;};
      
      AliHLTTPCPerformanceBase& operator*(){return *perf;}
      AliHLTTPCPerformanceBase* operator->(){return perf;}
    };
    vector<TSubPerformance> subPerformances;
//     vector<AliHLTTPCCAPerformanceBase*> subPerformances;
    
    const AliHLTTPCCAGBTracker *fTracker; // pointer to the tracker
#ifdef KFPARTICLE
    const KFParticleTopoReconstructor* fTopoReconstructor;
#endif
       /// MC information
    AliHLTResizableArray<AliHLTTPCCAHitLabel> fHitLabels; // array of hit MC labels
    AliHLTResizableArray<AliHLTTPCCAMCTrack> fMCTracks;   // array of MC tracks
    AliHLTResizableArray<AliHLTTPCCALocalMCPoint> fLocalMCPoints;   // array of MC points in slices CS


    int fStatNEvents; // n of events proceed

    TFile *fOutputFile;
    TDirectory *fHistoDir; // ROOT directory with histogramms
  
  private:
    void WriteDir2Current( TObject *obj );
  
    AliHLTTPCCAPerformance( const AliHLTTPCCAPerformance& );
    AliHLTTPCCAPerformance &operator=( const AliHLTTPCCAPerformance& );


 public:
#ifndef HLTCA_STANDALONE
  void ShiftHitsToMC(); // Use spreaded MCposition instead of hits
  void ResimulateHits(); // Create new hits spreading MCPositions
#endif // HLTCA_STANDALONE
  void RematchHits();// Match hits with closest MCPoint
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
