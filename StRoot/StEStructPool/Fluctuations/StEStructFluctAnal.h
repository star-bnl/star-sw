#ifndef __STESTRUCTFLUCTANAL__H
#define __STESTRUCTFLUCTANAL__H


#include "StTrackTopologyMap.h"
#include "multStruct.h"
#include "StEStructFluctuations.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPool/Correlations/StEStructPairCuts.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"

#undef TERMINUSSTUDY

class TFile;
class TH1F;
class TH1D;
class TH2F;
class StEStructEvent;
class StEStructTrack;
class StTimer;

class StEStructFluctAnal: public StEStructAnalysis {

 protected:

 public:

    int  manalysisMode;                 //! simple enumeration of analyses ...
    bool mskipPairCuts;                 //!
    bool mdoPairCutHistograms;          //!
    bool  mUseAllEtaTracks;             //!

    StEStructEvent      *mCurrentEvent;  //!  pointer to EStruct2pt data 
    StEStructPairCuts    mPair;          //! for pairs (1 at a time) and all pair cuts
    StEStructCentrality *mCentralities;

    int   doingPairCuts;
    int   etaSummingMode, phiSummingMode;
    float mEtaMin, mEtaMax;
    TH1F *hRefMultiplicity;
    TH1F *hMultiplicity;
    int   histosFilled;

    double *mptnplus;
    double *mptnminus;
    double *mptpplus;
    double *mptpminus;

    int   mTotBins;
    int   mnTotEvents, *mnCentEvents;
    int   mnCents, mnPts, mnPtCents;
    StEStructFluct **mFluct, **mPtFluct;

    int    nBins[NPHIBINS][NETABINS];
    int    offset[NPHIBINS][NETABINS];
    double fUnique[NPHIBINS][NETABINS];

  // Histogram declarations.
  // These will be summed over all events for all jobs.
  // We run a followup job to calculate \delta\sigma^2
    TH2F *hnBins;
    TH2F *hoffset;
    TH2F *hfUnique;

  // Here is the object I use to hold the binned tracks.
    multStruct      *ms;

    void  initHistograms();
    void  deleteHistograms();
    void  moveEvents();

    StEStructFluctAnal( int mode=4, int invokePairCuts = 0,
                        int etaSumMode=1, int phiSumMode=1 );
    virtual ~StEStructFluctAnal();

    StEStructPairCuts& getPairCuts();
    void  setAnalysisMode( int mode );
    void  setCutFile( const char* cutFileName );
    void  setEtaLimits( const char* cutFileName );
    void  adjustPtLimits( const char* cutFileName );
    void  setCentralityObject( StEStructCentrality *cent );

  //---> support of interface  
    bool loadUserCuts(const char* name, const char** vals, int nvals);
    virtual void setOutputFileName(const char* outFileName);
    bool  doEvent(StEStructEvent* p);
    void  init();
    void  cleanUp();
    void  finish() {};
    void  fillHistograms();
    void  writeHistograms(TFile* tf);
    void  writeQAHists(TFile* qatf);

  // analysis specific functions 
    void initCentralityObjects();
    void makeMultStruct();
    void AddEvent(multStruct *ms);
    int getEtaStart( int iEta, int dEta );
    int getPhiStart( int iPhi, int dPhi );
    int getNumEtaBins( int dEta );
    int getNumPhiBins( int dPhi );

    bool  doPairCuts();
    void  pairCuts(StEStructEvent* e1, StEStructEvent* e2, int j);


    ClassDef(StEStructFluctAnal,1)
};   

inline StEStructPairCuts& StEStructFluctAnal::getPairCuts() {
    return mPair;
};
inline void StEStructFluctAnal::setAnalysisMode(int mode){
    manalysisMode=mode;
};

inline void StEStructFluctAnal::setCutFile(const char* cutFileName){
    mPair.setCutFile(cutFileName);
    mPair.loadCuts();
    setEtaLimits(cutFileName);
    adjustPtLimits(cutFileName);
};
inline void StEStructFluctAnal::setCentralityObject(StEStructCentrality *cent) {
    mCentralities = cent;
    initCentralityObjects();
};

#endif


