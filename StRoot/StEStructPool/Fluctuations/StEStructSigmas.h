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

//>>>>> pp - AuAu difference.
#ifdef AUAUDATA
const int NSCENTBINS = 11;
const int NPTSCENTBINS = 3;
#endif
#ifdef HIJING
const int NSCENTBINS = 6;
const int NPTSCENTBINS = 2;
#endif
#ifdef PPDATA
const int NSCENTBINS = 1;
const int NPTSCENTBINS = 1;
#endif

const float SETAMIN = -1.0;
const float SETAMAX = +1.0;


class StEStructSigmas {

 protected:
    int histosFilled;
    int NPhiBins;
    int NEtaBins;

    TH2D *NSig[NSCENTBINS];
    TH2D *NDel[NSCENTBINS];
    TH2D *NPlus[NSCENTBINS];
    TH2D *NMinus[NSCENTBINS];
    TH2D *NPlusMinus[NSCENTBINS];
    TH2D *PSig[NSCENTBINS];
    TH2D *PPlus[NSCENTBINS];
    TH2D *PMinus[NSCENTBINS];
    TH2D *PPlusMinus[NSCENTBINS];
    TH2D *PNSig[NSCENTBINS];
    TH2D *PNPlus[NSCENTBINS];
    TH2D *PNMinus[NSCENTBINS];
    TH2D *PNPlusMinus[NSCENTBINS];
    TH2D *PNMinusPlus[NSCENTBINS];

    TH2D *SPtHat[NSCENTBINS];
    TH2D *PPtHat[NSCENTBINS];
    TH2D *MPtHat[NSCENTBINS];
    TH2D *sigSPtHat[NSCENTBINS];
    TH2D *sigPPtHat[NSCENTBINS];
    TH2D *sigMPtHat[NSCENTBINS];

    TH2D *ptNSig[NPTSCENTBINS][NPTBINS];
    TH2D *ptNDel[NPTSCENTBINS][NPTBINS];
    TH2D *ptNPlus[NPTSCENTBINS][NPTBINS];
    TH2D *ptNMinus[NPTSCENTBINS][NPTBINS];
    TH2D *ptNPlusMinus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPSig[NPTSCENTBINS][NPTBINS];
    TH2D *ptPPlus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPMinus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPPlusMinus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNSig[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNPlus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNMinus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNPlusMinus[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNMinusPlus[NPTSCENTBINS][NPTBINS];

    TH2D *ptSPtHat[NPTSCENTBINS][NPTBINS];
    TH2D *ptPPtHat[NPTSCENTBINS][NPTBINS];
    TH2D *ptMPtHat[NPTSCENTBINS][NPTBINS];
    TH2D *ptsigSPtHat[NPTSCENTBINS][NPTBINS];
    TH2D *ptsigPPtHat[NPTSCENTBINS][NPTBINS];
    TH2D *ptsigMPtHat[NPTSCENTBINS][NPTBINS];

    TH2D *NSigErrors[NSCENTBINS];
    TH2D *NDelErrors[NSCENTBINS];
    TH2D *NPlusErrors[NSCENTBINS];
    TH2D *NMinusErrors[NSCENTBINS];
    TH2D *NPlusMinusErrors[NSCENTBINS];
    TH2D *PSigErrors[NSCENTBINS];
    TH2D *PPlusErrors[NSCENTBINS];
    TH2D *PMinusErrors[NSCENTBINS];
    TH2D *PPlusMinusErrors[NSCENTBINS];
    TH2D *PNSigErrors[NSCENTBINS];
    TH2D *PNPlusErrors[NSCENTBINS];
    TH2D *PNMinusErrors[NSCENTBINS];
    TH2D *PNPlusMinusErrors[NSCENTBINS];
    TH2D *PNMinusPlusErrors[NSCENTBINS];

    TH2D *ptNSigErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptNDelErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptNPlusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptNMinusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptNPlusMinusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPSigErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPPlusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPMinusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPPlusMinusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNSigErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNPlusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNMinusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNPlusMinusErrors[NPTSCENTBINS][NPTBINS];
    TH2D *ptPNMinusPlusErrors[NPTSCENTBINS][NPTBINS];


    void  initArraysAndHistograms();
    void  deleteArraysAndHistograms();

 public:

    StEStructSigmas();
    virtual ~StEStructSigmas();

    void  fillHistograms();
    void  NHistograms();
    void  PHistograms();
    void  PNHistograms();
    void  ptNHistograms();
    void  ptPHistograms();
    void  ptPNHistograms();

    void  writeQAHists(TFile* qatf);


    ClassDef(StEStructSigmas,1)
};   


#endif


