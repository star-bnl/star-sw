//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAPerformanceBase.h,v 1.7 2010/08/17 15:47:13 ikulakov Exp $
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

class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class TDirectory;
class TH1D;
class TH2D;
class TProfile;

class TFile;

/**
 * @class AliHLTTPCCAPerformanceBase
 */
class AliHLTTPCCAPerformanceBase
{
  public:

    struct AliHLTTPCCAHitLabel {
      int fLab[3]; //* array of 3 MC labels
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

    vector<AliHLTTPCCAPerformanceMCTrackData>   &GetMCData()  { return mcData;   }; 
    vector<AliHLTTPCCAPerformanceRecoTrackData> &GetRecoData(){ return recoData; };
  
  protected:

    virtual void FillHistos();
    TH1D *GetHisto(string name);
    
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
        NGhostsHisto = 2,
        NGhostsProfiles = 0,
        NRecoTracksHisto = 2,
        NRecoTracksProfiles = 2,
        NHisto = NTracksPulls + NHitsPulls + NGhostsHisto + NGhostsProfiles + NRecoTracksHisto + NRecoTracksProfiles,
  };
    struct THistoInfo {
      const char *name;
      const char *title;
      Int_t n;
      Double_t l,r;
    };
    TH1D *fHistos[NHisto];
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
};

#endif
