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

const float SETAMIN = -1.0;
const float SETAMAX = +1.0;

class StEStructSigmas {

 protected:
 public:

    int mNPhiBins;
    int mNEtaBins;
    char *mKey;
    char *mpreFix;

    TH2D *NSig;
    TH2D *NDel;
    TH2D *NPlus;
    TH2D *NMinus;
    TH2D *NPlusMinus;
    TH2D *PSig;
    TH2D *PDel;
    TH2D *PPlus;
    TH2D *PMinus;
    TH2D *PPlusMinus;
    TH2D *PNSig;
    TH2D *PNDel;
    TH2D *PNPlus;
    TH2D *PNMinus;
    TH2D *PNPlusMinus;
    TH2D *PNMinusPlus;

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

    StEStructSigmas( char *key, int nPhi, int nEta, const char *preFix = "" );
    virtual ~StEStructSigmas();

    void  fillHistograms();
    void  NHistograms();
    void  PHistograms();
    void  PNHistograms();

    void  writeHistograms();


    ClassDef(StEStructSigmas,1)
};   


#endif


