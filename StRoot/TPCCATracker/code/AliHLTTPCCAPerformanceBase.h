//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAPerformanceBase.h,v 1.10 2010/09/01 10:38:27 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAPERFORMANCEBASE_H
#define ALIHLTTPCCAPERFORMANCEBASE_H

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCADef.h"
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
 * @class AliHLTTPCCAPerformanceBase
 */
class AliHLTTPCCAPerformanceBase
{
  public:

    struct AliHLTTPCCAHitLabel {
      int fLab[3]; //* array of 3 MC labels
      friend ostream& operator<<(ostream& out, const AliHLTTPCCAHitLabel& hl)
             { return out << hl.fLab[0] << " " << hl.fLab[1] << " "<< hl.fLab[2] << " " << std::endl;}
      friend istream& operator>>(istream& in, AliHLTTPCCAHitLabel& hl)
             { return in >> hl.fLab[0] >> hl.fLab[1] >> hl.fLab[2];}
    };

    AliHLTTPCCAPerformanceBase();
    virtual ~AliHLTTPCCAPerformanceBase();

    virtual void SetNewEvent( const AliHLTTPCCAGBTracker * const Tracker,
                 AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                 AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                 AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
    
      /// Histograms
    virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);
    void WriteHistos();

      /// Execute standart chain of procedures
    virtual void Exec(bool print = false);

    virtual void PrintEfficiencyStatistic(){ fEffStat.CalcEff(); fEffStat.Print(); };
    virtual void PrintEfficiency()         { fEff.CalcEff();     fEff.Print();     };

    virtual void Draw(){}; // draw diff things after performance
  
      /// Accessors
    AliHLTTPCEfficiencies &GetEff()    { return fEff;     };
    AliHLTTPCEfficiencies &GetEffStat(){ return fEffStat; };
    
    bool IsHistoCreated() { return fIsHistoCreated; }
    void SetHistoCreated(bool v = 1) { fIsHistoCreated = v; }

    vector<AliHLTTPCCAPerformanceMCTrackData>   &GetMCData()  { return mcData;   }; 
    vector<AliHLTTPCCAPerformanceRecoTrackData> &GetRecoData(){ return recoData; };
  
  protected:

    virtual void FillHistos();
    TH1 *GetHisto(const char* name);
    
      // Check if MC track is reconstructable. Calculate set of MC track. Etc.
    virtual void CheckMCTracks(){}; // fill mcData.
      // Find reco-MCTracks correspondence
    virtual void MatchTracks(){};   // fill recoData.
      // Calculate fEfficiencies
    virtual void EfficiencyPerformance();
      // Print fEfficiencies

    static void WriteDir2Current( TObject *obj );

    
    
    int fStatNEvents; //* n of events proceed

      /// Efficiencies
    AliHLTTPCEfficiencies fEff;
    AliHLTTPCEfficiencies fEffStat;


      /// Histos
  enum{ 
        NTracksPulls = 10,
        NHitsPulls = 4,
        
        NGhostsHisto = 4,
        NGhostsProfiles = 0,
        NGhosts2DHisto = 3,
        
        NRecoTracksHisto = 4,
        NRecoTracksProfiles = 2,
        NRecoTracks2DHisto = 3,
        
        NHisto = NTracksPulls + NHitsPulls
        + NGhostsHisto + NGhostsProfiles + NGhosts2DHisto
        + NRecoTracksHisto + NRecoTracksProfiles + NRecoTracks2DHisto
  };
  struct THistoInfo {
    THistoInfo(){};
    THistoInfo( const char *name_, const char *title_, Int_t nx_, Double_t left_, Double_t right_, Int_t ny_ = 0, Double_t low_ = 0, Double_t up_ = 0)
      :name(name_),title(title_),nx(nx_),left(left_),right(right_),ny(ny_),low(low_),up(up_){};
      
    const char *name;
    const char *title;
    Int_t nx;
    Double_t left,right;
    Int_t ny;
    Double_t low,up;
  };
  TH1 *fHistos[NHisto];
  THistoInfo fHistosInfo[NHisto];

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
    
    TDirectory *fHistoDir; //* ROOT directory with histogramm
    bool fIsHistoCreated;
};

#endif
