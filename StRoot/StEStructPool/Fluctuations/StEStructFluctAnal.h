#ifndef __STESTRUCTFLUCTANAL__H
#define __STESTRUCTFLUCTANAL__H


#include "StTrackTopologyMap.h"
#include "multStruct.h"
#include "StEStructFluctuations.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"
#include "StEStructPool/Correlations/StEStructPairCuts.h"

#include "TFile.h"
class TH1F;
class TH1D;
class TH2F;
class StEStructEvent;
class StEStructTrack;
class StTimer;
class StEStructQAHists;

class StEStructFluctAnal: public StEStructAnalysis {

 protected:

 public:
    StEStructFluctAnal( int mode=0, int etaSumMode=1, int phiSumMode=1 );
    virtual ~StEStructFluctAnal();

    StEStructPairCuts* getPairCuts();
    void  initStructures( StEStructCuts* tcut );
    void  setEtaLimits( StEStructCuts* tcut );
    void  setPtLimits( StEStructCuts* tcut );
    void  setQAHists( StEStructQAHists* qaHists );
    void  setPairCuts( StEStructPairCuts* cuts );

  //---> support of interface  
    virtual void setOutputFileName(const char* outFileName);
    bool  doEvent(StEStructEvent* p);
    void  finish();

  // analysis specific functions 
    void initHistograms();
    void deleteHistograms();
    void createCentralityObjects();
    void deleteCentralityObjects();
    void fillMultStruct();
    void AddEvent();
    int getEtaStart( int iEta, int dEta );
    int getPhiStart( int iPhi, int dPhi );
    int getNumEtaBins( int dEta );
    int getNumPhiBins( int dPhi );
    void   writeHistograms();
    void   writeQAHists(TFile* qatf);
    float etaOffset( float vz );


   // Member declarations.
    bool mAmDone;
    int  manalysisMode;                  //! simple enumeration of analyses ...
    char *moutFileName;
    int  mEtaSumMode, mPhiSumMode;

    StEStructEvent      *mCurrentEvent;  //!  pointer to EStruct2pt data 
    StEStructCentrality *mCentralities;

    StEStructPairCuts*     mPairCuts;      //! for pairs kine + all paircuts
    StEStructQAHists*      mQAHists;       //! for QA histogramming
    bool                   mlocalQAHists;  //! toggle needed for who writes out

    float mEtaMin, mEtaMax;
    float mPtMin,  mPtMax;

    double *mptnplus;
    double *mptnminus;
    double *mptpplus;
    double *mptpminus;

    int   mnTotBins;
    int   mnTotEvents, *mnCentEvents;
    int   mnCents, mnPts, mnPtCents;
    StEStructFluct **mFluct, **mPtFluct;

    int    nBins[NPHIBINS][NETABINS];
    int    offset[NPHIBINS][NETABINS];
    double fUnique[NPHIBINS][NETABINS];

  // Histogram declarations.
  // These will be summed over all events for all jobs.
  // We run followup jobs to calculate \delta\sigma^2 and plot
  // reference histograms.
    TH1F *hMultiplicity;
    TH1F *hMultiplicityBinned;
    TH1F *hPt;
    TH1F *hPtBinned;
    TH2F *hnBins;
    TH2F *hoffset;
    TH2F *hfUnique;

  // Here is the object I use to hold the binned tracks.
    multStruct *ms;

    ClassDef(StEStructFluctAnal,1)
};   

inline void StEStructFluctAnal::setOutputFileName(const char* outFileName) {
    if(!outFileName) return;
    moutFileName=new char[strlen(outFileName)+1];
    strcpy(moutFileName,outFileName);
}
inline void StEStructFluctAnal::setQAHists(StEStructQAHists* qahists){
  mQAHists = qahists;
}

inline void StEStructFluctAnal::setPairCuts(StEStructPairCuts* pcuts){
  mPairCuts=pcuts;
}
inline void StEStructFluctAnal::finish() {
    if (mAmDone) {
        cout << "StEStructFluctAnal::finish() has already been called." << endl;
        return;
    }
    TFile * tf=new TFile(moutFileName,"RECREATE");
    tf->cd();
    writeHistograms();
    tf->Close();
    mAmDone = true;
};
inline StEStructPairCuts* StEStructFluctAnal::getPairCuts() {
  return mPairCuts;
}

#endif
