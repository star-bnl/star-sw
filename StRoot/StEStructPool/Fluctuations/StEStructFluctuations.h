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

class StEStructFluct {

 protected:

 public:

  // For ease of I/O I put info into histograms.
  // Accumulating into histograms is very slow, so I
  // accumulate into arrays. Copy to histograms for
  // writing at the end.
    double *TotEvents[5];
    double *NSum[16];
    double *NDiff[2];
    double *NPlus[5];
    double *NMinus[5];
    double *NPlusMinus[8];
    double *PSum[16];
    double *PDiff[16];
    double *PPlus[11];
    double *PMinus[11];
    double *PPlusMinus[17];

    TH1D *hTotEvents[5];
    TH1D *hNSum[16];
    TH1D *hNDiff[2];
    TH1D *hNPlus[5];
    TH1D *hNMinus[5];
    TH1D *hNPlusMinus[8];
    TH1D *hPSum[16];
    TH1D *hPDiff[16];
    TH1D *hPPlus[11];
    TH1D *hPMinus[11];
    TH1D *hPPlusMinus[17];


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

    TH2F *EtaZNTracks;
    TH2F *EtaZNFitPoints;
    TH2F *EtaZNMaxPoints;
    TH2F *EtaZNFoundPoints;
    TH2F *InnerRow;
    TH2F *OuterRow;

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
    int   mTotBins;
    float mEtaMin, mEtaMax;

    void  initArrays();
    void  deleteArrays();
    void  initHistograms();
    void  deleteHistograms();

    StEStructFluct( char *key, int totBins, float EtaMin, float EtaMax );
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
    void fillEtaZ( float z, float eta,
                   int maxFitPoints, int foundPoints, int nFitPoints,
                   int iF, int iL );

    ClassDef(StEStructFluct,1)
};

#endif


