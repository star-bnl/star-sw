//StJetSkimEventMaker.h
//M.L. Miller (MIT)
//12/06

#ifndef STJETSKIMEVENTMAKER_H
#define STJETSKIMEVENTMAKER_H

#include "StMaker.h"
#include <string>
using namespace std;

#include <TRef.h>

class TFile;
class StMuDstMaker;
class StMCAsymMaker;
class TTree;
class StJetSkimEvent;
class StJetSkimTrig;
class StJetSkimTrigHeader;

class StJetSkimEventMaker : public StMaker 
{
public:
	
    StJetSkimEventMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
    virtual ~StJetSkimEventMaker();
    
    virtual Int_t Init();
    virtual Int_t InitRun(int runnumber);
    virtual Int_t Make();
    virtual Int_t Finish();
    virtual void Clear(const Option_t*);
    
    TTree* tree() const { return mTree; }
    StJetSkimEvent* event() const { return mEvent; }
    
    void addSimuTrigger(int trigId) {mSimuTrigIds.push_back(trigId);}

    const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StJetSkimEventMaker.h,v 1.9 2017/01/02 15:33:26 rfatemi Exp $ built " __DATE__ " " __TIME__; return cvs;}
    	
protected:
    StMuDstMaker*   muDstMaker;         //!
    StMCAsymMaker*  mcAsymMaker;        //!
    	
private:
    const char*     outName;            //!
    TFile*          mOutfile;           //!
    TTree*          mTree;              //!
    StJetSkimEvent* mEvent;             //!
    TRef            mCurrentHeaderRef;  //!
    vector<int>     mSimuTrigIds;       //!
    bool            isRealData;         //!
	
  void fillTriggerSimulationInfo(StJetSkimTrig &trig);
  void fillThresholds(StJetSkimTrigHeader &header);
	
    ClassDef(StJetSkimEventMaker,0)
};


#endif // STJETSKIMEVENTMAKER_H
