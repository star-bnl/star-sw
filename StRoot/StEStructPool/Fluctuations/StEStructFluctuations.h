#define HIJING

#ifndef __STESTRUCTFLUCTUATIONS__H
#define __STESTRUCTFLUCTUATIONS__H


#include "multStruct.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPool/Correlations/StEStructPairCuts.h"

#undef TERMINUSSTUDY

class TFile;
class TH1F;
class TH1D;
class TH2F;
class StEStructEvent;
class StEStructTrack;
class StTimer;

//>>>>> pp - AuAu difference.
#ifdef AUAUDATA
const int NCENTBINS = 11;
const int NPTCENTBINS = 3;
#endif
#ifdef HIJING
const int NCENTBINS = 6;
const int NPTCENTBINS = 2;
#endif
#ifdef PPDATA
const int NCENTBINS = 1;
const int NPTCENTBINS = 1;
#endif

const float ETAMIN = -1.0;
const float ETAMAX = +1.0;


class StEStructFluctuations: public StEStructAnalysis {

 protected:

 public:

    int  manalysisMode; //! simple enumeration of analyses ...
    bool mskipPairCuts; //!
    bool mdoPairCutHistograms; //!

    StEStructEvent*        mCurrentEvent;  //!  pointer to EStruct2pt data 
    StEStructPairCuts      mPair; //! for pairs (1 at a time) and all pair cuts
    StTimer*  mtimer;       //!

    int   doingPairCuts;
    int   etaSummingMode, phiSummingMode;
    int   histosFilled;

    int   TOTBINS;
    int   nTotEvents, nCentEvents[NCENTBINS];

    int    nBins[NPHIBINS][NETABINS];
    int    offset[NPHIBINS][NETABINS];
    double fUnique[NPHIBINS][NETABINS];

  // Histogram declarations.
  // These will be summed over all events for all jobs.
  // We run a followup job to calculate \delta\sigma^2
    TH2F *hnBins;
    TH2F *hoffset;
    TH2F *hfUnique;

  // For ease of I/O I put info into histograms.
  // Accumulating into histograms is very slow, so I
  // accumulate into arrays. Copy to histograms for
  // writing at the end.
    double *TotEvents[NCENTBINS][5];
    double *NSum[NCENTBINS][2];
    double *NDiff[NCENTBINS][2];
    double *NPlus[NCENTBINS][2];
    double *NMinus[NCENTBINS][2];
    double *NPlusMinus[NCENTBINS];
    double *PSum[NCENTBINS][5];
    double *PDiff[NCENTBINS][8];
    double *PPlus[NCENTBINS][5];
    double *PMinus[NCENTBINS][5];
    double *PPlusMinus[NCENTBINS][8];
    double *PNSum[NCENTBINS][4];
    double *PNDiff[NCENTBINS][4];
    double *PNPlus[NCENTBINS][4];
    double *PNMinus[NCENTBINS][4];
    double *PNPlusMinus[NCENTBINS][8];

    TH1D *hTotEvents[NCENTBINS][5];
    TH1D *hNSum[NCENTBINS][2];
    TH1D *hNDiff[NCENTBINS][2];
    TH1D *hNPlus[NCENTBINS][2];
    TH1D *hNMinus[NCENTBINS][2];
    TH1D *hNPlusMinus[NCENTBINS];
    TH1D *hPSum[NCENTBINS][5];
    TH1D *hPDiff[NCENTBINS][8];
    TH1D *hPPlus[NCENTBINS][5];
    TH1D *hPMinus[NCENTBINS][5];
    TH1D *hPPlusMinus[NCENTBINS][8];
    TH1D *hPNSum[NCENTBINS][4];
    TH1D *hPNDiff[NCENTBINS][4];
    TH1D *hPNPlus[NCENTBINS][4];
    TH1D *hPNMinus[NCENTBINS][4];
    TH1D *hPNPlusMinus[NCENTBINS][8];

    double *ptTotEvents[NPTCENTBINS][NPTBINS][5];
    double *ptNSum[NPTCENTBINS][NPTBINS][2];
    double *ptNDiff[NPTCENTBINS][NPTBINS][2];
    double *ptNPlus[NPTCENTBINS][NPTBINS][2];
    double *ptNMinus[NPTCENTBINS][NPTBINS][2];
    double *ptNPlusMinus[NPTCENTBINS][NPTBINS];
    double *ptPSum[NPTCENTBINS][NPTBINS][5];
    double *ptPDiff[NPTCENTBINS][NPTBINS][8];
    double *ptPPlus[NPTCENTBINS][NPTBINS][5];
    double *ptPMinus[NPTCENTBINS][NPTBINS][5];
    double *ptPPlusMinus[NPTCENTBINS][NPTBINS][8];
    double *ptPNSum[NPTCENTBINS][NPTBINS][4];
    double *ptPNDiff[NPTCENTBINS][NPTBINS][4];
    double *ptPNPlus[NPTCENTBINS][NPTBINS][4];
    double *ptPNMinus[NPTCENTBINS][NPTBINS][4];
    double *ptPNPlusMinus[NPTCENTBINS][NPTBINS][8];

    TH1D *hptTotEvents[NPTCENTBINS][NPTBINS][5];
    TH1D *hptNSum[NPTCENTBINS][NPTBINS][2];
    TH1D *hptNDiff[NPTCENTBINS][NPTBINS][2];
    TH1D *hptNPlus[NPTCENTBINS][NPTBINS][2];
    TH1D *hptNMinus[NPTCENTBINS][NPTBINS][2];
    TH1D *hptNPlusMinus[NPTCENTBINS][NPTBINS];
    TH1D *hptPSum[NPTCENTBINS][NPTBINS][5];
    TH1D *hptPDiff[NPTCENTBINS][NPTBINS][8];
    TH1D *hptPPlus[NPTCENTBINS][NPTBINS][5];
    TH1D *hptPMinus[NPTCENTBINS][NPTBINS][5];
    TH1D *hptPPlusMinus[NPTCENTBINS][NPTBINS][8];
    TH1D *hptPNSum[NPTCENTBINS][NPTBINS][4];
    TH1D *hptPNDiff[NPTCENTBINS][NPTBINS][4];
    TH1D *hptPNPlus[NPTCENTBINS][NPTBINS][4];
    TH1D *hptPNMinus[NPTCENTBINS][NPTBINS][4];
    TH1D *hptPNPlusMinus[NPTCENTBINS][NPTBINS][8];

  // Histogram declarations.
  // These are summed over events for this job.
  // Primarily intended for cross checks.
#ifdef TERMINUSSTUDY
    TH1F *sum[NPHIBINS][NETABINS];
    TH1F *plus[NPHIBINS][NETABINS];
    TH1F *minus[NPHIBINS][NETABINS];
    TH1F *plusminus[NPHIBINS][NETABINS];
    TH1F *diff[NPHIBINS][NETABINS][NCENTBINS];
#endif

    TH2F *occNSum[NCENTBINS];
    TH2F *occNPlus[NCENTBINS];
    TH2F *occNMinus[NCENTBINS];
    TH2F *occNDiff[NCENTBINS];
    TH2F *occPSum[NCENTBINS];
    TH2F *occPPlus[NCENTBINS];
    TH2F *occPMinus[NCENTBINS];
    TH2F *occPDiff[NCENTBINS];
    TH2F *occPNSum[NCENTBINS];
    TH2F *occPNPlus[NCENTBINS];
    TH2F *occPNMinus[NCENTBINS];
    TH2F *occPNDiff[NCENTBINS];
    TH2F *occptNSum[NPTCENTBINS][NPTBINS];
    TH2F *occptNPlus[NPTCENTBINS][NPTBINS];
    TH2F *occptNMinus[NPTCENTBINS][NPTBINS];
    TH2F *occptNDiff[NPTCENTBINS][NPTBINS];
    TH2F *occptPSum[NPTCENTBINS][NPTBINS];
    TH2F *occptPPlus[NPTCENTBINS][NPTBINS];
    TH2F *occptPMinus[NPTCENTBINS][NPTBINS];
    TH2F *occptPDiff[NPTCENTBINS][NPTBINS];
    TH2F *occptPNSum[NPTCENTBINS][NPTBINS];
    TH2F *occptPNPlus[NPTCENTBINS][NPTBINS];
    TH2F *occptPNMinus[NPTCENTBINS][NPTBINS];
    TH2F *occptPNDiff[NPTCENTBINS][NPTBINS];

    TH1F *multNSum[NCENTBINS];
    TH1F *multNPlus[NCENTBINS];
    TH1F *multNMinus[NCENTBINS];
    TH1F *multNDiff[NCENTBINS];
    TH1F *multPSum[NCENTBINS];
    TH1F *multPPlus[NCENTBINS];
    TH1F *multPMinus[NCENTBINS];
    TH1F *multPDiff[NCENTBINS];
    TH1F *multptNSum[NPTCENTBINS][NPTBINS];
    TH1F *multptNPlus[NPTCENTBINS][NPTBINS];
    TH1F *multptNMinus[NPTCENTBINS][NPTBINS];
    TH1F *multptNDiff[NPTCENTBINS][NPTBINS];
    TH1F *multptPSum[NPTCENTBINS][NPTBINS];
    TH1F *multptPPlus[NPTCENTBINS][NPTBINS];
    TH1F *multptPMinus[NPTCENTBINS][NPTBINS];
    TH1F *multptPDiff[NPTCENTBINS][NPTBINS];

  // Here is the object I use to hold the binned tracks.
    multStruct      *ms;

    void  initArrays();
    void  deleteArrays();
    void  initHistograms();
    void  deleteHistograms();
    void  moveEvents();

    StEStructFluctuations(int mode=0, int invokePairCuts = 0,
                          int etaSumMode=1, int phiSumMode=1);
    StEStructFluctuations(const char* cutFileName, int mode=0,
                          int invokePairCuts = 0,
                          int etaSumMode=1, int phiSumMode=1);
    virtual ~StEStructFluctuations();

    StEStructPairCuts& getPairCuts();
    void  setAnalysisMode(int mode);
    void  setCutFile(const char* cutFileName);  

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
    int  getCentBin( int mult );
    int  getPtCentBin( int jCent );
    int  getPtBin( float pt );
    void makeMultStruct();
    void constantMultStruct(int nt, float val);
    void randomMultStruct(double p, float val);
    void AddEvent(multStruct *ms);
    void AddToPtBin( int iCent, int iPt, int iBin,
                     double plus,    double minus,
                     double pplus,   double pminus,
                     double psqplus, double psqminus );
    void AddToBin( int iCent,      int iBin,
                   double plus,    double minus,
                   double pplus,   double pminus,
                   double psqplus, double psqminus );
    int getEtaStart( int iEta, int dEta );
    int getPhiStart( int iPhi, int dPhi );
    int getNumEtaBins( int dEta );
    int getNumPhiBins( int dPhi );

    bool  doPairCuts();
    void  pairCuts(StEStructEvent* e1, StEStructEvent* e2, int j);


    ClassDef(StEStructFluctuations,1)
};   

inline StEStructPairCuts& StEStructFluctuations::getPairCuts() {
    return mPair;
};
inline void StEStructFluctuations::setAnalysisMode(int mode){
    manalysisMode=mode;
};

inline void StEStructFluctuations::setCutFile(const char* cutFileName){
    mPair.setCutFile(cutFileName);
    mPair.loadCuts();
};

#endif


