//StiDefaultEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiDefaultEvaluator_HH
#define StiDefaultEvaluator_HH

//std
#include <string>
using std::string;

//ROOT
#include "TObject.h"

//StiDefaultEvaluator
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

class StiDefaultEvaluator : public StiEvaluator
{
public:
    static StiDefaultEvaluator* instance(const string val="empty");
    static void kill();
    
    friend class nobody;
    
    void evaluate(const StiTrackContainer*);
    
    StiDefaultEvaluator(); //Not implemented
    StiDefaultEvaluator(const string&); //Must pass file-name
    virtual ~StiDefaultEvaluator(); 
    static StiDefaultEvaluator* sinstance;
    
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
