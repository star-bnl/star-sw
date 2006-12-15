//StJetSkimEventMaker.h
//M.L. Miller (MIT)
//12/06

#ifndef StJetSkimEventMaker_HH
#define StJetSkimEventMaker_HH

#include "StMaker.h"
#include <string>
using namespace std;

class TFile;
class StMuDstMaker;
class TTree;
class StJetSkimEvent;

class StJetSkimEventMaker : public StMaker 
{
public:
	
    StJetSkimEventMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
	virtual ~StJetSkimEventMaker();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
	virtual void Clear(const Option_t*);
    
    TTree* tree();
    	
protected:
		StMuDstMaker*   muDstMaker;   //!
	
private:
	const char*     outName;      //!
    TFile           *mOutfile;   //!
    TTree           *mTree;      //!
	StJetSkimEvent* mEvent; //!
	
    ClassDef(StJetSkimEventMaker,0)
};


#endif
