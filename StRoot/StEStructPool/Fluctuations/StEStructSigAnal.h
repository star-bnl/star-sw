
#ifndef __STESTRUCTSIGANAL__H
#define __STESTRUCTSIGANAL__H


#include "multStruct.h"
#include "StEStructSigmas.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "Stiostream.h"

class TFile;
class TH2F;

class StEStructSigAnal {

 protected:
 public:
    int   mNPhiBins;
    int   mNEtaBins;
    float mNFiles;
    float mEtaMin, mEtaMax;
    int   mnSigmas, mnCents, mnPts, mnPtCents;
    StEStructSigmas **mSigma;
    char *mInputFile;
    TFile *mInFile;
    char *mpreFix;

    void  initArrays();
    void  deleteArrays();

    StEStructSigAnal( char *inputFile, char *prefix = "" );
    virtual ~StEStructSigAnal();

    void newFile( char *inputFile );
    void closeFile();
    void getLimits();
    void normalizeCounters();
    void fillHistograms();
    void writeHistograms( TFile* sig );

    ClassDef(StEStructSigAnal,1)
};   


#endif
