//StiEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiEvaluator_HH
#define StiEvaluator_HH

//forward declarations (must #include these in the source file)
class StiTrackContainer;
class TFile;
class TNtuple;
class TTree;
class StMcTrack;
class StTrack;
class StiTrack;
class StTrackPairInfo;


//Temp class to be stored in TTree, eventually move to it's own .h, .cxx files
#include "TObject.h"

class TClonesArray;

class TrackEntry
{
public:
    TrackEntry();
    virtual ~TrackEntry() {};

    void setMcTrack(StMcTrack*);
    void setGlobalTrack(StTrack*);
    void setStiTrack(StiTrack*);

    double getMcTrackId();
    double getMcTrackPt();

    void clear();

private:

    //temp kinematic info : MC
    double mcTrackId;
    double mcTrackPsi;
    double mcTrackPt;
    double mcTrackChi2;
    
    //temp kinematic info : global
    short  globalTrackQ;
    double globalTrackM;
    double globalTrackPt;
    double globalTrackPsi;
    double globalTrackChi2;
    double globalTrackNHit;
    
    //temp kinematic info : Sti
    double stiTrackM;
    double stiTrackPt;
    double stiTrackPsi;
    double stiTrackChi2;
    double stiTrackNHit;

    ClassDef(TrackEntry,1) 
};

class StiEvaluator
{
 public:
    static StiEvaluator* instance();
    static void kill();

    friend class nobody;

    void evaluateForEvent(const StiTrackContainer*);
    
 private:
    //singleton stuff
    StiEvaluator();
    virtual ~StiEvaluator();
    static StiEvaluator* sinstance;

 private:
    void build();
    
    TFile* mFile;
    TTree* mTree;
    TrackEntry* mEntry;
};

#endif
