#ifndef StAngleCorrAnalysisManager_HH
#define StAngleCorrAnalysisManager_HH

///////////////////////////////////////////////////////////////////////////////
//
// StAngleCorrAnalysisManager
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "StEvent.h"
#include "StAngleCorrAnalysis.h"
#include <vector>
#include "TString.h"
#include "TStopwatch.h"

class StAngleCorrAnalysisManager {

private:
    int mNumberOfTracksInPool,mNumberOfEventsInPool;
    vector<StAngleCorrAnalysis*> vec;
     TStopwatch* stopwatch;
 
public:
                         StAngleCorrAnalysisManager();
                         ~StAngleCorrAnalysisManager();
    void                 AddAnalysis(TString analysisName);
    StAngleCorrAnalysis* GetAnalysis(TString analysisName);
    void                 ProcessEvent(StEvent& ev);
    void                 DoEvents(StEvent& ev);
    void                 DoSignals(); 
    void                 DoBackgrounds();
    void                 WriteDiagnostic();
};

#endif
