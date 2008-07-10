//StJetSkimEventMaker.h
//M.L. Miller (MIT)
//12/06

#ifndef StJetSkimEventMaker_HH
#define StJetSkimEventMaker_HH

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
    
    TTree* tree();
    
    void addSimuTrigger(int trigId) {mSimuTrigIds.push_back(trigId);}

    const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StJetSkimEventMaker.h,v 1.5 2008/07/10 01:20:14 tai Exp $ built "__DATE__" "__TIME__; return cvs;}
    	
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


#endif
