//StUpsilonFilter.h

#ifndef StUpsilonFilter_HH
#define StUpsilonFilter_HH

#include "TObject.h"
#include "AnaCuts.h"

class StMuEvent;
class StUpsilonMuEvent;
class StMuDstMaker;
class StMuTrack;
class TFile;
class TTree;

class StUpsilonFilter
{
public:
    enum ioType {kWrite=0, kRead=1};
    
    StUpsilonFilter(ioType, const char* file);
    virtual ~StUpsilonFilter();
    
    //set the cuts
    void setCuts(const AnaCuts& c);
    
    //Get the action
    StUpsilonMuEvent* event() const {return mEvent;}
    
    //number of events in the tree
    int nEvents() const; 
    
    //action
    void fill(StMuDstMaker* maker=0); //we need maker for the filtering process, not reading

private:
    StUpsilonFilter(); //Not implemented
    
private:
    ioType mIoType;
    int mEventCounter;
    
    TFile* mFile; //!
    TTree* mTree; //!
    StUpsilonMuEvent* mEvent; //!
    
    ClassDef(StUpsilonFilter,1)
	};

#endif
