//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCPERFORMANCEBASE_H
#define ALIHLTTPCPERFORMANCEBASE_H

#include "AliHLTTPCCounters.h"

#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include <fstream>
#include <cstdio>
#include <map>

#ifndef HLTCA_STANDALONE
#include "TString.h"
#endif
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

struct AliHLTTPCCAHitLabel {
  int fLab[3]; //* array of 3 MC labels
  friend ostream& operator<<(ostream& out, const AliHLTTPCCAHitLabel& hl)
  { return out << hl.fLab[0] << " " << hl.fLab[1] << " "<< hl.fLab[2] << " " << std::endl;}
  friend istream& operator>>(istream& in, AliHLTTPCCAHitLabel& hl)
  { return in >> hl.fLab[0] >> hl.fLab[1] >> hl.fLab[2];}
};

namespace // copy of same function in CADef.h. To remove warnings
{
  template<typename T1, typename T2>
  void UNUSED_PARAM2_( const T1 &, const T2 & ) {}
}

/**
 * @class AliHLTTPCPerformanceBase
 */
class AliHLTTPCPerformanceBase
{
 public:

  AliHLTTPCPerformanceBase();
  virtual ~AliHLTTPCPerformanceBase();

  virtual void SetNewEvent( const AliHLTTPCCAGBTracker * const Tracker,
  AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
  AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);

    /// Execute standart chain of procedures
  virtual void Exec(bool print = false);

  virtual void PrintEfficiencyStatistic(){ fEffStat.CalcEff(); fEffStat.Print(); };
  virtual void PrintEfficiency()         { fEff.CalcEff();     fEff.Print();     };

#ifndef HLTCA_STANDALONE
    /// Histograms
  virtual void CreateHistos(string histoDir = "", TFile* outFile = 0){ UNUSED_PARAM2_(histoDir, outFile); };
    
  bool IsHistoCreated() { return fIsHistoCreated; }
  void SetHistoCreated(bool v = 1) { fIsHistoCreated = v; }

  virtual void Draw(){}; // draw diff things after performance
#endif
  
    /// Accessors
  AliHLTTPCEfficiencies &GetEff()    { return fEff;     };
  AliHLTTPCEfficiencies &GetEffStat(){ return fEffStat; };

  vector<AliHLTTPCCAPerformanceMCTrackData>   &GetMCData()  { return mcData;   }; 
  vector<AliHLTTPCCAPerformanceRecoTrackData> &GetRecoData(){ return recoData; };
  
 public:

#ifndef HLTCA_STANDALONE
  virtual void FillHistos(){};
  TH1 *GetHisto(int iHisto);
#endif
  
    // Check if MC track is reconstructable. Calculate set of MC track. Etc.
  virtual void CheckMCTracks(){}; // fill mcData.
    // Find reco-MCTracks correspondence
  virtual void MatchTracks(){};   // fill recoData.
    // Calculate fEfficiencies
  virtual void EfficiencyPerformance();
    // Print fEfficiencies


  int fStatNEvents; //* n of events proceed

    /// Efficiencies
  AliHLTTPCEfficiencies fEff;
  AliHLTTPCEfficiencies fEffStat;

#ifndef HLTCA_STANDALONE
    /// Histos
  int NHisto;
  struct THistoInfo {
    THistoInfo(){};
    THistoInfo( const char *name_, const char *title_, Int_t nx_, Double_t left_, Double_t right_,
                Int_t ny_ = 0, Double_t low_ = 0, Double_t up_ = 0, TString XAxisName_="", TString YAxisName_="")
      :name(name_),title(title_),nx(nx_),left(left_),right(right_),ny(ny_),low(low_),up(up_),XAxisName(XAxisName_),YAxisName(YAxisName_){};
      
    const char *name;
    const char *title;
    Int_t nx;
    Double_t left,right;
    Int_t ny;
    Double_t low,up;
    TString XAxisName, YAxisName;
  };
  TH1 **fHistos; // array of histos
  THistoInfo *fHistosInfo; // array of histos parameters
#endif
  
    /// Track information
  vector<AliHLTTPCCAPerformanceMCTrackData>   mcData;   // iMCTrack to trackInfo map.
  vector<AliHLTTPCCAPerformanceRecoTrackData> recoData; // iRecoTrack to trackInfo map.

    /// Reco information
  const AliHLTTPCCAGBTracker *fTracker; //* pointer to the tracker

    /// MC information
  AliHLTResizableArray<AliHLTTPCCAHitLabel> *fHitLabels; //* array of hit MC labels
  AliHLTResizableArray<AliHLTTPCCAMCTrack> *fMCTracks;   //* array of MC tracks
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *fLocalMCPoints;   //* array of MC points in slices CS

  int nRecoTracks, nMCTracks;
  
#ifndef HLTCA_STANDALONE
  TDirectory *fHistoDir; //* ROOT directory with histogramm
  bool fIsHistoCreated;
#endif
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
