//StJetFilter.h

#ifndef StJetFilter_HH
#define StJetFilter_HH

#include "TObject.h"
#include "AnaCuts.h"

class StMuEvent;
class StJetMuEvent;
class StMuDstMaker;
class StMuTrack;
class TFile;
class TTree;

class StJetFilter
{
public:
    enum ioType {kWrite=0, kRead=1};
    
    StJetFilter(ioType, const char* file);
    virtual ~StJetFilter();
    
    //set the cuts
    void setCuts(const AnaCuts& c);
    
    //Get the action
    StJetMuEvent* event() const {return mEvent;}
    
    //number of events in the tree
    int nEvents() const; 
    
    //action
    void fill(StMuDstMaker* maker=0); //we need maker for the filtering process, not reading

private:
    StJetFilter(); //Not implemented
    
private:
    ioType mIoType;
    int mEventCounter;
    
    TFile* mFile; //!
    TTree* mTree; //!
    StJetMuEvent* mEvent; //!
    
    ClassDef(StJetFilter,1)
	};

#endif
