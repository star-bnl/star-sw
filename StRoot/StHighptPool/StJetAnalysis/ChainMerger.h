//ChainMerger.h

#ifndef ChainMerger_HH
#define ChainMerger_HH

#include "TObject.h"
#include <string>
using namespace std;

class TChain;
class StUpsilonMuEvent;

class ChainMerger
{
public:
    ChainMerger(const char* dir, const char* outfile);
    virtual ~ChainMerger();

    //access
    TChain* chain() const {return mChain;}
    
    //Action
    int findEntries(string infile);
    
private:
    ChainMerger(); //Not implemented
    void buildChain(string dir, string outfile);
    
private:
    TChain* mChain; //!
    StUpsilonMuEvent* mEvent; //!
        
    ClassDef(ChainMerger,1)
	};

#endif
