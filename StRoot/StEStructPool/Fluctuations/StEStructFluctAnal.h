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

class StEStructFluctAnal: public StEStructAnalysis {

 protected:

 public:
    StEStructFluctAnal( int mode=0, int etaSumMode=1, int phiSumMode=1 );
    virtual ~StEStructFluctAnal();

    StEStructPairCuts& getPairCuts();
    void  setCutFile(   const char* cutFileName, StEStructCentrality *cent );
    void  setEtaLimits( const char* cutFileName );
    void  setPtLimits(  const char* cutFileName );

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
    int  manalysisMode;                  //! simple enumeration of analyses ...
    char *moutFileName;
    int  mEtaSumMode, mPhiSumMode;

    StEStructEvent      *mCurrentEvent;  //!  pointer to EStruct2pt data 
    StEStructCentrality *mCentralities;

    StEStructPairCuts    mPair; //! Simply so I can support getPairCuts and doEstruct macro doesn't barf.

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
inline void StEStructFluctAnal::finish() {
    TFile * tf=new TFile(moutFileName,"RECREATE");
    tf->cd();
    writeHistograms();
    tf->Close();
};
inline StEStructPairCuts& StEStructFluctAnal::getPairCuts() {
  return mPair;
}

#endif
