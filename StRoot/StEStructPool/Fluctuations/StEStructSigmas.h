#define AUAUDATA

#ifndef __STESTRUCTSIGMAS__H
#define __STESTRUCTSIGMAS__H


#include "multStruct.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "Stiostream.h"

class TFile;
class TH1F;
class TH1D;
class TH2F;
class TH2D;

class StEStructSigmas {

 protected:
 public:

    int   mNPhiBins;
    int   mNEtaBins;
    float mEtaMin, mEtaMax;
    char *mKey;
    char *mpreFix;

    TH2D *NSig;
    TH2D *NDel;
    TH2D *NPlus;
    TH2D *NMinus;
    TH2D *NPlusMinus;

    // Have different measures of pt fluctuations.
    // First is based on \Delta\sigma^2.
    // Second is based on \Phi_{p_\perp}.
    // Third is based on \sigma^2_{dynamic}.
    TH2D *PSig[3];
    TH2D *PDel[3];
    TH2D *PPlus[3];
    TH2D *PMinus[3];
    TH2D *PPlusMinus[3];
    TH2D *PNSig[3];
    TH2D *PNDel[3];
    TH2D *PNPlus[3];
    TH2D *PNMinus[3];
    TH2D *PNPlusMinus[3];
    TH2D *PNMinusPlus[3];

    TH2D *SPtHat;
    TH2D *PPtHat;
    TH2D *MPtHat;
    TH2D *sigSPtHat;
    TH2D *sigPPtHat;
    TH2D *sigMPtHat;

    TH2D *NSigErrors;
    TH2D *NDelErrors;
    TH2D *NPlusErrors;
    TH2D *NMinusErrors;
    TH2D *NPlusMinusErrors;
    TH2D *PSigErrors;
    TH2D *PDelErrors;
    TH2D *PPlusErrors;
    TH2D *PMinusErrors;
    TH2D *PPlusMinusErrors;
    TH2D *PNSigErrors;
    TH2D *PNDelErrors;
    TH2D *PNPlusErrors;
    TH2D *PNMinusErrors;
    TH2D *PNPlusMinusErrors;
    TH2D *PNMinusPlusErrors;

    TH2D *sigSPtHatErrors;
    TH2D *sigPPtHatErrors;
    TH2D *sigMPtHatErrors;

    void  initHistograms();
    void  deleteHistograms();

    StEStructSigmas( char *key,
                     int   nPhi,   int   nEta,
                     float EtaMin, float EtaMax,
                     const char *preFix = "" );
    virtual ~StEStructSigmas();

    void  fillHistograms();
    void  NHistograms();
    void  PHistograms();
    void  PNHistograms();

    void  writeHistograms();


    ClassDef(StEStructSigmas,1)
};   


#endif


