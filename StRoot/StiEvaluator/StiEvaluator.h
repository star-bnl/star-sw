//StiEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiEvaluator_HH
#define StiEvaluator_HH

#include <string>
using std::string;

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

//Temp class to be stored in TTree, eventually move to it's own .h, .cxx files
#include "TObject.h"

class TClonesArray;

class StiHitEntry : public TObject
{
public:
    StiHitEntry();
    virtual ~StiHitEntry();
    
    void reset();
    
    //Might as well make these all public, for now
    
    //These quantities come from the hit itself, *not* track location
    double hitPosition; //StiHit::position()
    double hitRefAngle; //StiHit::refAngle()
    double hitLocalX;
    double hitLocalY;
    double hitLocalZ;
    
    //Get these from StiHit->globalPosition().x, .y(), .z()
    double hitGlobalX;
    double hitGlobalY;
    double hitGlobalZ;
    
    //These quantities come from the track-node location
    double trackAlpha; //rotation of local frame w.r.t. global 
    double trackLocalX;
    double trackLocalY;
    double trackLocalZ;
    double trackLocalEta;
    double trackLocalCurvature;
    double trackLocalTanLambda;
    double trackLocalChi2;
    double trackXCenter; //global (x,y) of center of circle
    double trackYCenter;
    
private:
    ClassDef(StiHitEntry, 1)
};


class TrackEntry
{
public:
    TrackEntry();
    virtual ~TrackEntry() {};
    
    void setMcTrack(StMcTrack*);
    void setGlobalTrack(StTrack*);
    void setStiTrack(StiTrack*);
    
    void addStiHitEntry(const StiHitEntry&);
    
    double getMcTrackId();
    double getMcTrackPt();

    unsigned int hitCounter() const {return mHitCounter;}
    TClonesArray& array() const {return *mArray;}
    
    void clear();
    
private:
    //Counter:
    unsigned int mHitCounter;
    TClonesArray* mArray;
    
    //temp kinematic info : MC
    double mcTrackId;
    double mcTrackPsi;
    double mcTrackRapidity;
    double mcTrackE;
    double mcTrackPx;
    double mcTrackPy;
    double mcTrackPz;
    double mcTrackPt;
    double mcTrackEta;
    
    //temp kinematic info : global
    short  globalTrackQ;
    double globalTrackM;
    double globalTrackPsi;
    double globalTrackChi2;
    double globalTrackNHit;
    double globalTrackPx;
    double globalTrackPy;
    double globalTrackPz;
    double globalTrackPt;
    double globalTrackEta;
    double globalTrackFitPoints;
    
    //temp kinematic info : Sti
    double stiTrackM;
    double stiTrackQ;
    double stiTrackPsi;
    double stiTrackChi2;
    double stiTrackNHit;
    double stiTrackY;
    double stiTrackTanL;
    double stiTrackPx;
    double stiTrackPy;
    double stiTrackPz;
    double stiTrackPt;
    double stiTrackEta;
    
    
    ClassDef(TrackEntry,1) 
};

class StiEvaluator
{
public:
    static StiEvaluator* instance(const string val="empty");
    static void kill();
    
    friend class nobody;
    
    void evaluateForEvent(const StiTrackContainer*);
    
private:
    //singleton stuff
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
