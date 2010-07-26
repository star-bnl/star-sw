//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAPerformanceBase.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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
    virtual void CreateHistos(string histoDir = "");
    void WriteHistos();

      /// Execute standart chain of procedures
    virtual void Exec(bool print = false);

    virtual void PrintEfficiencyStatistic(){ fEffStat.CalcEff(); fEffStat.Print(); };

      /// Accessors
    AliHLTTPCEfficiencies &GetEff()    { return fEff;     };
    AliHLTTPCEfficiencies &GetEffStat(){ return fEffStat; };

  protected:

    virtual void FillHistos(){};
    TH1D *GetHisto(string name);
    
      // Check if MC track is reconstructable. Calculate set of MC track. Etc.
    virtual void CheckMCTracks(){}; // fill mcData.
      // Find reco-MCTracks correspondence
    virtual void MatchTracks(){};   // fill recoData.
      // Calculate fEfficiencies
    virtual void EfficiencyPerformance(){};
      // Print fEfficiencies
    virtual void PrintEfficiency()         { fEff.CalcEff();     fEff.Print();     };

    static void WriteDir2Current( TObject *obj );

    
    
    int fStatNEvents; //* n of events proceed

      /// Efficiencies
    AliHLTTPCEfficiencies fEff;
    AliHLTTPCEfficiencies fEffStat;


      /// Histos
    enum{ NHisto = 10 };
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
