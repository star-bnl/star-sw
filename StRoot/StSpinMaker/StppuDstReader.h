
//StppuDstReader.h
//M.L. Miller (Yale Software)
//10/02

#ifndef StppuDstReader_HH
#define stppuDstReader_HH

#include <map>
using std::map;
#include <string>
using std::string;

#include "TObject.h"
#include "StppEvent.h"

class TTree;
class TBranch;
//class StppEvent;
class StJets;

class StppuDstReader
{
public:
    typedef StppEvent::StJetsMap StJetsMap;
    
    StppuDstReader(TTree* theTree);
    virtual ~StppuDstReader() {};
    
    int numberOfEvents() const;
    StppEvent* getEvent(int i);

    TObjArray branchNames();
    
private:
    StppuDstReader();
    
    StppEvent* mEvent;
    TTree* mTree;
    
    StJetsMap mStJetsMap; //!
    
    ClassDef(StppuDstReader,1)
	};

#endif
