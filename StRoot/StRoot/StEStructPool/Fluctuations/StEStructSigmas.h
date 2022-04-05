#define AUAUDATA

#ifndef __STESTRUCTSIGMAS__H
#define __STESTRUCTSIGMAS__H

#include <TNtuple.h>
#include "multStruct.h"
#include "../AnalysisMaker/StEStructAnalysis.h"
#include <stdlib.h>
#include <stdio.h>

class TFile;
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TNtuple;

class StEStructSigmas {

 protected:
 public:

    int   mNPhiBins, mPhiSumMode;
    int   mNEtaBins, mEtaSumMode;
    float mEtaMin, mEtaMax;
    char *mKey;
    char *mpreFix;

    TH2D *NSig;
    TH2D *NDel;
    TH2D *NPlus;
    TH2D *NMinus;
    TH2D *NPlusMinus;
    TH2D *NSigCorrection;
    TH2D *NDelCorrection;
    TH2D *NPlusCorrection;
    TH2D *NMinusCorrection;
    TH2D *NPlusMinusCorrection;

    // Have different measures of pt fluctuations.
    // First is based on \Delta\sigma^2.
    // Second is based on \Phi_{p_\perp}.
    // Third is based on \sigma^2_{dynamic}.
    TH2D *PSig[4];
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

    TNtuple *binTuple;
    TNtuple *scaleTuple;
    TNtuple *sumTuple;

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

    int getEtaStart( int iEta, int dEta );
    int getPhiStart( int iPhi, int dPhi );
    int getNumEtaBins( int dEta );
    int getNumPhiBins( int dPhi );

    ClassDef(StEStructSigmas,1)
};   

struct binTupleStruct {
    float type;
    float phiScale;
    float etaScale;
    float phi;
    float eta;
    float sig2;
    float sig2_1;
    float sig2_2;
    float nbar;
    float events;
    float f3;
};
struct scaleTupleStruct {
    float type;
    float phiScale;
    float etaScale;
    float A;
    float B;
    float nBins;
    float f3;
    float f3sq;
    float sig2;
    float sig2f3;
};
struct sumTupleStruct {
    float type;
    float B;
    float nBins;
};


#endif


