#define AUAUDATA

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

const float ETAMIN = -1.0;
const float ETAMAX = +1.0;

class StEStructFluct {

 protected:

 public:

  // For ease of I/O I put info into histograms.
  // Accumulating into histograms is very slow, so I
  // accumulate into arrays. Copy to histograms for
  // writing at the end.
    double *TotEvents[5];
    double *NSum[2];
    double *NDiff[2];
    double *NPlus[2];
    double *NMinus[2];
    double *NPlusMinus;
    double *PSum[5];
    double *PDiff[8];
    double *PPlus[5];
    double *PMinus[5];
    double *PPlusMinus[8];
    double *PNSum[4];
    double *PNDiff[4];
    double *PNPlus[4];
    double *PNMinus[4];
    double *PNPlusMinus[8];

    TH1D *hTotEvents[5];
    TH1D *hNSum[2];
    TH1D *hNDiff[2];
    TH1D *hNPlus[2];
    TH1D *hNMinus[2];
    TH1D *hNPlusMinus;
    TH1D *hPSum[5];
    TH1D *hPDiff[8];
    TH1D *hPPlus[5];
    TH1D *hPMinus[5];
    TH1D *hPPlusMinus[8];
    TH1D *hPNSum[4];
    TH1D *hPNDiff[4];
    TH1D *hPNPlus[4];
    TH1D *hPNMinus[4];
    TH1D *hPNPlusMinus[8];


    TH2F *occNSum;
    TH2F *occNPlus;
    TH2F *occNMinus;
    TH2F *occNDiff;
    TH2F *occPSum;
    TH2F *occPPlus;
    TH2F *occPMinus;
    TH2F *occPDiff;
    TH2F *occPNSum;
    TH2F *occPNPlus;
    TH2F *occPNMinus;
    TH2F *occPNDiff;

    TH1F *multNSum;
    TH1F *multNPlus;
    TH1F *multNMinus;
    TH1F *multNDiff;
    TH1F *multPSum;
    TH1F *multPPlus;
    TH1F *multPMinus;
    TH1F *multPDiff;

  // Here is the object I use to hold the binned tracks.
    multStruct      *ms;
    
  // mKey is a unique string which we use to identify histograms that
  // belong to this object. Important when reading histograms produced
  // by this object.
    char *mKey;
    int mTotBins;

    void  initArrays();
    void  deleteArrays();
    void  initHistograms();
    void  deleteHistograms();

    StEStructFluct( char *key, int totBins );
    virtual ~StEStructFluct();


  //---> support of interface
    void  fillHistograms();
    void  writeHistograms();
    void  writeQAHistograms();

  // analysis specific functions 
    void makeMultStruct();
    void AddEvent(multStruct *ms);
    void AddToBin( int iBin,
                   double plus,    double minus,
                   double pplus,   double pminus,
                   double psqplus, double psqminus );
    void fillOccupancies( double dPhi,  double dEta,
                          double nPlus, double nMinus,
                          double pPlus, double pMinus );
    void fillMults( double nPlus, double nMinus,
                    double pPlus, double pMinus );

    ClassDef(StEStructFluct,1)
};

#endif


