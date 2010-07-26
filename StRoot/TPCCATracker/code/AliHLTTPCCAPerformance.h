//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAPerformance.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAPERFORMANCE_H
#define ALIHLTTPCCAPERFORMANCE_H

#include "AliHLTTPCCAPerformanceBase.h"

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

    typedef AliHLTTPCCAPerformanceBase::AliHLTTPCCAHitLabel AliHLTTPCCAHitLabel;

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
    void SetTracker( AliHLTTPCCAGBTracker* const Tracker ){ fTracker = Tracker; };
    const AliHLTTPCCAMCTrack &MCTrack(int i) const { return fMCTracks[i]; }
    const AliHLTTPCCAHitLabel &HitLabel(int i) const { return fHitLabels[i]; }
    
  protected:

          /// Histograms
    void CreateHistos();
    void WriteHistos();
    
          /// Read\write MC information
    void ReadMCEvent( FILE *in );
    void ReadLocalMCPoints( FILE *in );

    void WriteMCEvent( FILE *out ) const;

      /// Sub-pefromances
    struct TSubPerformance{
      AliHLTTPCCAPerformanceBase* perf;
      string name;

      TSubPerformance(){};
      TSubPerformance(AliHLTTPCCAPerformanceBase* perf_, string name_){
        perf = perf_;
        name = name_;
      };
//       ~TSubPerformance(){if (perf) delete perf;};
      
      AliHLTTPCCAPerformanceBase& operator*(){return *perf;}
      AliHLTTPCCAPerformanceBase* operator->(){return perf;}
    };
    vector<TSubPerformance> subPerformances;
//     vector<AliHLTTPCCAPerformanceBase*> subPerformances;
    
    const AliHLTTPCCAGBTracker *fTracker; //* pointer to the tracker

      /// MC information
    AliHLTResizableArray<AliHLTTPCCAHitLabel> fHitLabels; //* array of hit MC labels
    AliHLTResizableArray<AliHLTTPCCAMCTrack> fMCTracks;   //* array of MC tracks
    AliHLTResizableArray<AliHLTTPCCALocalMCPoint> fLocalMCPoints;   //* array of MC points in slices CS


    int fStatNEvents; //* n of events proceed

    TDirectory *fHistoDir; //* ROOT directory with histogramms

  private:
    AliHLTTPCCAPerformance( const AliHLTTPCCAPerformance& );
    AliHLTTPCCAPerformance &operator=( const AliHLTTPCCAPerformance& );
};

#endif
