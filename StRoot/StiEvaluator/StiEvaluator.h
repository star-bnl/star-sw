//StiEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiEvaluator_HH
#define StiEvaluator_HH

//std
#include <string>
using std::string;

//ROOT
#include "TObject.h"

//StiEvaluator
#include "TreeEntryClasses.h"
#include "../Sti/StiEvaluator.h"

//forward declarations (must #include these in the source file)
class StiTrackContainer;
class TFile;
class TNtuple;
class TTree;
class StMcTrack;
class StTrack;
class StiTrack;
class StTrackPairInfo;
class StiKalmanTrack;
class StiHit;
class StiKalmanTrackNode;
class trackPing;

class StiEvaluator
{
public:
    static StiEvaluator* instance(const string val="empty");
    static void kill();
    
    friend class nobody;
    
    void evaluate(const StiTrackContainer*);
    
    StiEvaluator(); //Not implemented
    StiEvaluator(const string&); //Must pass file-name
    virtual ~StiEvaluator(); 
    static StiEvaluator* sinstance;
    
private:
    void build();
    
    void fillTree(StiTrack*, StTrackPairInfo*);
    void fillHitEntry(const StiKalmanTrackNode*);
    void fillHitEntry(const StiHit*);
    void fillHitEntry(const StiKalmanTrack*);
      
    string mFileName;
    TFile* mFile;
    TTree* mTree;
    TrackEntry* mEntry;
    StiHitEntry mStiHitEntry;
};

#endif
