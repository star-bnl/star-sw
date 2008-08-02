#ifndef STJETHISTMAKER_H
#define STJETHISTMAKER_H
#include "StMaker.h"

#include "TH3.h"
#include "TH2.h"

#include <string>
#include <iostream>
#include <fstream>
#include <map>
#include <algorithm>
using namespace std;

class TFile;
class TTree;
class TNtuple;
class StMuDst;
class StMuDstMaker;
class StEmcADCtoEMaker;
class StBemcTables;

class StEmcSimulatorMaker;


class StJetHistMaker : public StMaker
{
public:
    
    //StJetHistMaker(const Char_t *name, StMuDstMaker* uDstMaker, StEmcADCtoEMaker* adc2e, StEmcSimulatorMaker* sim, const char *outputName); 
    StJetHistMaker(StMuDstMaker* uDstMaker, const char *outputName);
    
    virtual Int_t Init();
    virtual Int_t InitRun(Int_t);
    virtual Int_t Make();
    virtual Int_t Finish();

    //just put the histograms in public:
    TH2* mbVertexZvsNp;
    TH3* mbTrackKin;
    TH2* mbNfitVsEta;

    TH2* ht1VertexZvsNp;
    TH3* ht1TrackKin;
    TH2* ht1NfitVsEta;
    
    TH2* otherVertexZvsNp;
    TH3* otherTrackKin;
    TH2* otherNfitVsEta;
    
    TH2* mipHistVsEta;
    TH2* mipEvsEta;
    TH2* towerEvsId;
    TH2* towerAdcvsId;

    
protected:
    void InitFile();
    void FinishFile();
    void fillBarrelHits();

	
    StMuDstMaker*   muDstMaker;   //!
    //!StEmcSimulatorMaker* mEmcSim; //!
	
    const char* mOutName;      //!
    StMuDst* mudst;        //!
    //!StEmcADCtoEMaker* mAdc2E; //!
    StBemcTables* mTables;//!
    TFile* mOutfile;   //!
    
    ClassDef(StJetHistMaker,0)
	};

#endif // STJETHISTMAKER_H
